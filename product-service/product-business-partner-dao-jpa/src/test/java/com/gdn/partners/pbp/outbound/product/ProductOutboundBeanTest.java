package com.gdn.partners.pbp.outbound.product;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.mockito.ArgumentMatchers.eq;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.ProductBrandUpdateRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.response.ProductMasterDataUpdateResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.enums.ErrorMessage;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.PageRequest;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.notification.dto.GdnRestSimpleResponse;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.outbound.xProduct.feign.XProductFeign;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.MasterCatalogRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionCategoryMappingRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionIdsRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodesSkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionItemInfoResponse;
import com.gdn.x.productcategorybase.dto.response.DimensionsAndUomResponse;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;

public class ProductOutboundBeanTest {

  @InjectMocks
  private ProductOutboundBean productOutboundBean;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private XProductFeign xProductFeign;

  @Captor
  private ArgumentCaptor<List<ConfigurationStatusRequest>> configurationStatusRequestListArgumentCaptor;

  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String CATEGORY_CODE = "category-code";
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String CATEGORY_NAME = "category-name";
  private static final String MERCHANT_SKU = "merchant-sku-1";
  private static final String ITEM_SKU = "item-sku-1";
  private static final String PRODUCT_CODE = "PRD-123456";
  private static final String OLD_PRODUCT_CODE = "MTA-0123456";
  private static final String PRODUCT_ID = "PRODUCT_ID";

  private static final String OFFLINE_ITEM_ID = "oofline-item-id";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String EXTERNAL_PICKUP_POINT_CODE = "external-pickup-point-code";
  private static final Double OFFER_PRICE = 10000.0;
  
  // Constants for getOmniChannelSkuToItemCode tests
  private static final String SELLER_CODE = "SELLER123";
  private static final String OMNI_CHANNEL_SKU_1 = "OMNI-SKU-001";
  private static final String OMNI_CHANNEL_SKU_2 = "OMNI-SKU-002";
  private static final String ITEM_CODE_1 = "ITEM-001";
  private static final String ITEM_CODE_2 = "ITEM-002";

  private static final String ERROR_MSG = "error_msg";
  private static final String KEYWORD = "keyword";
  private static final String ERROR_CODE = "error_code";
  private static final long TIME = 1000;
  private static final String LOCATION_PATH = "locationPath";
  private static final String BRAND_CODE ="brand_code";
  private static final String BUSINESS_PARTNER_CODE = "BLI-00001";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String ITEM_NAME = "itemName";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String PREDICTION_ID = "predictionId";
  private static final String SKU_CODE = "skuCode";
  private static final String UPC_CODE = "upcCode";

  private List<OfflineItemRequest> offlineItemRequests = new ArrayList();
  private GdnRestSingleResponse<OfflineItemResponse> successOfflineItemResponse;
  private GdnRestSingleResponse<OfflineItemResponse> failedResponse;
  private GdnRestListResponse<UpsertOfflineItemPriceResponse> gdnRestListResponse;

  private UpsertOfflineItemRequest upsertOfflineItemRequest;
  private List<UpsertOfflineItemRequest> upsertOfflineItemRequests;
  private UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse;
  private List<UpsertOfflineItemPriceResponse> upsertOfflineItemPriceResponses;
  private GdnRestListResponse<UpsertOfflineItemPriceResponse> upsertOfflineItemPriceListResponse;

  private List<String> merchantSkus = new ArrayList();
  private List<String> productIds = Arrays.asList(PRODUCT_ID, PRODUCT_CODE);
  GdnRestListResponse<ProductImageResponse> gdnRestSingleResponse = new GdnRestListResponse<>();
  private CategorySummaryResponse categorySummaryResponse;
  private ProductActivateImageRequest productActivateImageRequest;
  private ActivateImageResponse activateImageResponse;
  private ProductRequest productRequest = new ProductRequest();
  private ProductImageEditRequest productImageEditRequest;
  private CategoryMultipleIdRequest categoryMultipleIdRequest;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    merchantSkus.add(MERCHANT_SKU);

    OfflineItemResponseDetail offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setItemSku(ITEM_SKU);
    offlineItemResponseDetail.setMerchantSku(MERCHANT_SKU);

    List<OfflineItemResponseDetail> offlineItemResponseDetails = new ArrayList<>();
    offlineItemResponseDetails.add(offlineItemResponseDetail);

    OfflineItemResponse offlineItemResponse = new OfflineItemResponse();
    offlineItemResponse.setOfflineProducts(offlineItemResponseDetails);

    successOfflineItemResponse = new GdnRestSingleResponse<>(offlineItemResponse, REQUEST_ID);

    failedResponse = new GdnRestSingleResponse(ERROR_MSG, ERROR_CODE, false, null, REQUEST_ID);

    gdnRestListResponse = new GdnRestListResponse(null, new PageMetaData(0, 0, 100), REQUEST_ID);

    upsertOfflineItemRequest = new UpsertOfflineItemRequest();
    upsertOfflineItemRequest.setOfflineItemId(OFFLINE_ITEM_ID);
    upsertOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    upsertOfflineItemRequest.setOfferPrice(OFFER_PRICE);
    upsertOfflineItemRequest.setItemSku(ITEM_SKU);

    upsertOfflineItemRequests = new ArrayList<>();
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);

    upsertOfflineItemPriceResponse = new UpsertOfflineItemPriceResponse();
    upsertOfflineItemPriceResponse.setOfflineItemId(OFFLINE_ITEM_ID);
    upsertOfflineItemPriceResponse.setPickupPointCode(PICKUP_POINT_CODE);
    upsertOfflineItemPriceResponse.setOfferPrice(OFFER_PRICE);
    upsertOfflineItemPriceResponse.setItemSku(ITEM_SKU);
    upsertOfflineItemPriceResponse.setSuccess(true);

    upsertOfflineItemPriceResponses = new ArrayList<>();
    upsertOfflineItemPriceResponses.add(upsertOfflineItemPriceResponse);

    upsertOfflineItemPriceListResponse =
        new GdnRestListResponse<>(upsertOfflineItemPriceResponses, new PageMetaData(0, 0, 100),
            REQUEST_ID);

    categorySummaryResponse = new CategorySummaryResponse();
    categorySummaryResponse.setCategoryName(CATEGORY_NAME);
    categorySummaryResponse.setCategoryCode(CATEGORY_CODE);

    productActivateImageRequest = new ProductActivateImageRequest();

    activateImageResponse = new ActivateImageResponse();

    productImageEditRequest = new ProductImageEditRequest();
    categoryMultipleIdRequest = new CategoryMultipleIdRequest();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign);
    Mockito.verifyNoMoreInteractions(xProductFeign);
  }

  @Test
  public void getOfflineItems() throws Exception {
    Mockito.when(
        this.xProductFeign.getOfflineItemsByMerchantCodeAndMerchantSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
            new SimpleListStringRequest(merchantSkus))).thenReturn(successOfflineItemResponse);

    List<OfflineItemResponseDetail> actual =
        this.productOutboundBean.findOfflineItems(REQUEST_ID, USERNAME, MERCHANT_CODE, merchantSkus);

    Mockito.verify(this.xProductFeign)
        .getOfflineItemsByMerchantCodeAndMerchantSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
            new SimpleListStringRequest(merchantSkus));
  }

  @Test
  public void getOfflineItems_emptyMerchantSkus() throws Exception {
    List<OfflineItemResponseDetail> actual = this.productOutboundBean.findOfflineItems(REQUEST_ID,
        USERNAME, MERCHANT_CODE, null);
    Assertions.assertTrue(actual.isEmpty());
  }

  @Test
  public void getOfflineItems_fail() throws Exception {
    Mockito.when(
        this.xProductFeign.getOfflineItemsByMerchantCodeAndMerchantSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
            new SimpleListStringRequest(merchantSkus))).thenReturn(failedResponse);

    try {
      List<OfflineItemResponseDetail> actual =
          this.productOutboundBean.findOfflineItems(REQUEST_ID, USERNAME, MERCHANT_CODE, merchantSkus);
    } catch (ApplicationRuntimeException e) {
      Mockito.verify(this.xProductFeign)
          .getOfflineItemsByMerchantCodeAndMerchantSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
              Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
              new SimpleListStringRequest(merchantSkus));
    }
  }

  @Test
  public void getOfflineItems_Valid_SuccessWithNullValue() throws Exception {

    failedResponse.setSuccess(true);

    Mockito.when(this.xProductFeign
        .getOfflineItemsByMerchantCodeAndMerchantSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
            new SimpleListStringRequest(merchantSkus))).thenReturn(failedResponse);

    try{
      List<OfflineItemResponseDetail> actual = this.productOutboundBean.findOfflineItems(REQUEST_ID, USERNAME, MERCHANT_CODE, merchantSkus);
    }catch (ApplicationRuntimeException e) {
      Mockito.verify(this.xProductFeign)
          .getOfflineItemsByMerchantCodeAndMerchantSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
              Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
              new SimpleListStringRequest(merchantSkus));
    }
  }

  @Test
  public void upsertOfflineItemPrice() throws Exception {
    Mockito.when(this.xProductFeign
        .upsertOfflineItemPrice(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, offlineItemRequests))
        .thenReturn(gdnRestListResponse);

    List<UpsertOfflineItemPriceResponse> actual = this.productOutboundBean
        .upsertOfflineItemPrice(REQUEST_ID, USERNAME, MERCHANT_CODE, offlineItemRequests);

    Mockito.verify(this.xProductFeign)
        .upsertOfflineItemPrice(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, offlineItemRequests);

  }

  @Test
  public void upsertOfflineItemPrice_failed() throws Exception {

    gdnRestListResponse.setSuccess(false);

    Mockito.when(this.xProductFeign
        .upsertOfflineItemPrice(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, offlineItemRequests))
        .thenReturn(gdnRestListResponse);

    List<UpsertOfflineItemPriceResponse> actual = this.productOutboundBean
        .upsertOfflineItemPrice(REQUEST_ID, USERNAME, MERCHANT_CODE, offlineItemRequests);

    Mockito.verify(this.xProductFeign)
        .upsertOfflineItemPrice(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, offlineItemRequests);

  }

  @Test
  public void upsertOfflineItemPrice_exception() throws Exception {

    gdnRestListResponse.setSuccess(false);

    OfflineItemRequest offlineItemRequest = new OfflineItemRequest();
    offlineItemRequest.setOfflineItemId(OFFLINE_ITEM_ID);
    offlineItemRequest.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);
    offlineItemRequest.setItemSku(ITEM_SKU);
    offlineItemRequest.setMerchantSku(MERCHANT_SKU);
    offlineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    offlineItemRequest.setOfferPrice(OFFER_PRICE);
    offlineItemRequests.add(offlineItemRequest);

    UpsertOfflineItemPriceResponse expected = new UpsertOfflineItemPriceResponse();
    expected.setOfflineItemId(OFFLINE_ITEM_ID);
    expected.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);
    expected.setItemSku(ITEM_SKU);
    expected.setMerchantSku(MERCHANT_SKU);
    expected.setPickupPointCode(PICKUP_POINT_CODE);
    expected.setOfferPrice(OFFER_PRICE);
    expected.setSuccess(Boolean.FALSE);
    expected.setErrorMessage(ERROR_MSG);

    Mockito.when(this.xProductFeign
        .upsertOfflineItemPrice(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, offlineItemRequests))
        .thenThrow(new RuntimeException(ERROR_MSG));

    List<UpsertOfflineItemPriceResponse> actual = this.productOutboundBean
        .upsertOfflineItemPrice(REQUEST_ID, USERNAME, MERCHANT_CODE, offlineItemRequests);

    Mockito.verify(this.xProductFeign)
        .upsertOfflineItemPrice(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, offlineItemRequests);
    Assertions.assertEquals(expected, actual.get(0));
  }

  @Test
  public void findSummaryInstantPickupByFilter_valid_success() throws Exception {
    ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
    PageRequest pageRequest = PageRequest.of(0, 10);
    SortOrder sortOrder = new SortOrder("orderBy", "sortBy");
    GdnRestListResponse<ItemSummaryResponse> response = new GdnRestListResponse<>();
    response.setSuccess(true);
    Mockito.when(
        xProductFeign.getListOfItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), any(), any(),
            any())).thenReturn(response);
    productOutboundBean.findSummaryInstantPickupByFilter(itemSummaryRequest, pageRequest, sortOrder);
    Mockito.verify(xProductFeign)
        .getListOfItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), any(), any(),
            any());
  }

  @Test
  public void findSummaryInstantPickupByFilter_nullSortAndSuccessFalse_error() throws Exception {
    ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
    PageRequest pageRequest = PageRequest.of(0, 10);
    GdnRestListResponse<ItemSummaryResponse> response = new GdnRestListResponse<>();
    response.setSuccess(false);
    Mockito.when(
        xProductFeign.getListOfItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), any(), any(),
            any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        productOutboundBean.findSummaryInstantPickupByFilter(itemSummaryRequest, pageRequest, null);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(xProductFeign)
        .getListOfItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), any(),
            any(), any());
  }

  @Test
  public void updateProductMasterCatalog_Valid_Success() throws Exception {
    Mockito.when(xProductFeign.updateProductMasterCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any()))
        .thenReturn(gdnRestListResponse);
    productOutboundBean.updateProductMasterCatalog(REQUEST_ID, USERNAME, new MasterCatalogRequest(), PRODUCT_CODE);
    Mockito.verify(xProductFeign)
        .updateProductMasterCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), any());
  }

  @Test
  public void updateProductMasterCatalog_ResponseFalse_ThrowsException() throws Exception {
    gdnRestListResponse.setSuccess(false);
    Mockito.when(xProductFeign.updateProductMasterCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any()))
        .thenReturn(gdnRestListResponse);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        productOutboundBean.updateProductMasterCatalog(REQUEST_ID, USERNAME, new MasterCatalogRequest(), PRODUCT_CODE);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(xProductFeign)
        .updateProductMasterCatalog(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any());
  }

  @Test
  public void addProductAttribute_Valid_Success() throws Exception {
    Mockito.when(xProductFeign.addProductAttribute(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any()))
        .thenReturn(gdnRestListResponse);
    productOutboundBean.addProductAttribute(REQUEST_ID, USERNAME, new ProductAttributeRequest(), PRODUCT_CODE);
    Mockito.verify(xProductFeign)
        .addProductAttribute(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), any());
  }

  @Test
  public void upsertOfflineItemTest() throws Exception {
    Mockito.when(this.xProductFeign.upsertOfflineItem(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        upsertOfflineItemRequests)).thenReturn(upsertOfflineItemPriceListResponse);

    List<UpsertOfflineItemPriceResponse> actual =
        this.productOutboundBean.upsertOfflineItem(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests);

    Mockito.verify(this.xProductFeign)
        .upsertOfflineItem(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests);

  }

  @Test
  public void upsertOfflineItemTest_failed() throws Exception {

    upsertOfflineItemPriceListResponse.setSuccess(false);

    Mockito.when(this.xProductFeign.upsertOfflineItem(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        upsertOfflineItemRequests)).thenReturn(upsertOfflineItemPriceListResponse);

    List<UpsertOfflineItemPriceResponse> actual =
        this.productOutboundBean.upsertOfflineItem(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests);

    Mockito.verify(this.xProductFeign)
        .upsertOfflineItem(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests);

  }

  @Test
  public void upsertOfflineItemTest_exception() throws Exception {

    upsertOfflineItemPriceListResponse.setSuccess(false);

    UpsertOfflineItemPriceResponse expected = new UpsertOfflineItemPriceResponse();
    expected.setOfflineItemId(OFFLINE_ITEM_ID);
    expected.setItemSku(ITEM_SKU);
    expected.setPickupPointCode(PICKUP_POINT_CODE);
    expected.setOfferPrice(OFFER_PRICE);
    expected.setSuccess(Boolean.FALSE);
    expected.setErrorMessage(ERROR_MSG);
    expected.setCncActive(true);

    Mockito.when(this.xProductFeign.upsertOfflineItem(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        upsertOfflineItemRequests)).thenThrow(new RuntimeException(ERROR_MSG));

    List<UpsertOfflineItemPriceResponse> actual =
        this.productOutboundBean.upsertOfflineItem(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests);

    Mockito.verify(this.xProductFeign)
        .upsertOfflineItem(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests);
    Assertions.assertEquals(expected, actual.get(0));
  }

  @Test
  public void getReviewConfigurationChangesTest() {
    List<ConfigurationStatusResponse> configurationStatusResponses = new ArrayList<>();
    GdnRestListResponse<ConfigurationStatusResponse> response =
        new GdnRestListResponse<>(configurationStatusResponses, new PageMetaData(10, 0, 10), REQUEST_ID);
    Mockito.when(pcbFeign.getConfigurationChanges(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(TIME))).thenReturn(response);
    productOutboundBean.getReviewConfigurationChanges(TIME);
    Mockito.verify(pcbFeign).getConfigurationChanges(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(TIME));
  }

  @Test
  public void getReviewConfigurationChangesExceptionTest() {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        new GdnRestListResponse<>(ERROR_MSG, ERROR_CODE, false, REQUEST_ID);
    Mockito.when(pcbFeign.getConfigurationChanges(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(TIME))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getReviewConfigurationChanges(TIME);
      });
    } finally {
      Mockito.verify(pcbFeign).getConfigurationChanges(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
          Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(TIME));
    }
  }

  @Test
  public void getReviewConfigurationChangesEmptyResultTest() {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        new GdnRestListResponse<>(null, new PageMetaData(10, 0, 10), REQUEST_ID);
    Mockito.when(pcbFeign.getConfigurationChanges(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(TIME))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getReviewConfigurationChanges(TIME);
      });
    } finally {
      Mockito.verify(pcbFeign).getConfigurationChanges(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
          Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(TIME));
    }
  }

  @Test
  public void republishProduct() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse();
    gdnBaseRestResponse.setSuccess(true);
    Mockito.when(pcbFeign.republishProduct(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
        Mockito.eq(Constants.UPDATE_OPERATION_TYPE), Mockito.eq((Arrays.asList(PRODUCT_CODE)))))
        .thenReturn(gdnBaseRestResponse);
    boolean response = productOutboundBean.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(pcbFeign).republishProduct(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
        Mockito.eq(Constants.UPDATE_OPERATION_TYPE), Mockito.eq((Arrays.asList(PRODUCT_CODE))));
    Assertions.assertTrue(response);
  }

  @Test
  public void updateProductCategoryTest() {
    GdnRestSingleResponse<CategorySummaryResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categorySummaryResponse, REQUEST_ID);
    Mockito.when(pcbFeign.updateProductCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(PRODUCT_CODE),
        Mockito.eq(CATEGORY_CODE), Mockito.anyBoolean(),Mockito.anyBoolean())).thenReturn(gdnRestSingleResponse);
    CategorySummaryResponse categorySummaryResponse =
        productOutboundBean.updateProductCategory(PRODUCT_CODE, CATEGORY_CODE, true,true);
    Mockito.verify(pcbFeign).updateProductCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(PRODUCT_CODE),
        Mockito.eq(CATEGORY_CODE), Mockito.anyBoolean(),Mockito.anyBoolean());
    Assertions.assertEquals(CATEGORY_CODE, categorySummaryResponse.getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, categorySummaryResponse.getCategoryName());
  }


  @Test
  public void updateProductCategoryExceptionTest() {
    GdnRestSingleResponse<CategorySummaryResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID);
    Mockito.when(pcbFeign.updateProductCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(PRODUCT_CODE),
        Mockito.eq(CATEGORY_CODE), Mockito.anyBoolean(),Mockito.anyBoolean())).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateProductCategory(PRODUCT_CODE, CATEGORY_CODE, true,true);
      });
    } finally {
      Mockito.verify(pcbFeign).updateProductCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
          Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(PRODUCT_CODE),
          Mockito.eq(CATEGORY_CODE), Mockito.anyBoolean(),Mockito.anyBoolean());
    }
  }

  @Test
  public void updateProductCategoryApplicationRuntimeExceptionTest() {
    GdnRestSingleResponse<CategorySummaryResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ErrorCategory.VALIDATION.getMessage(), ErrorCategory.VALIDATION.getCode(), false,
            null, REQUEST_ID);
    Mockito.when(pcbFeign.updateProductCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(PRODUCT_CODE),
        Mockito.eq(CATEGORY_CODE), Mockito.anyBoolean(),Mockito.anyBoolean())).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateProductCategory(PRODUCT_CODE, CATEGORY_CODE,true,true);
      });
    } finally {
      Mockito.verify(pcbFeign).updateProductCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
          Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(PRODUCT_CODE),
          Mockito.eq(CATEGORY_CODE), Mockito.anyBoolean(),Mockito.anyBoolean());
    }
  }

  @Test
  public void republishProductExceptionTest() {
    Mockito.when(pcbFeign.republishProduct(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
        Mockito.eq(Constants.UPDATE_OPERATION_TYPE), Mockito.eq((Arrays.asList(PRODUCT_CODE)))))
        .thenReturn(new GdnBaseRestResponse());
    try {
      productOutboundBean.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(pcbFeign).republishProduct(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
          Mockito.eq(Constants.UPDATE_OPERATION_TYPE), Mockito.eq((Arrays.asList(PRODUCT_CODE))));
    }
  }

  @Test
  public void getReviewConfigurationTest() {
    List<ConfigurationStatusResponse> configurationStatusResponses = new ArrayList<>();
    GdnRestListResponse<ConfigurationStatusResponse> response =
        new GdnRestListResponse<>(configurationStatusResponses, new PageMetaData(10, 0, 10), REQUEST_ID);
    Mockito.when(pcbFeign.getReviewConfiguration(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
        Mockito.anyList())).thenReturn(response);
    productOutboundBean.getReviewConfiguration(Collections.singletonList(
        ConfigurationStatusRequest.builder().categoryCode(CATEGORY_CODE).businessPartnerCode(MERCHANT_CODE).build()));
    Mockito.verify(pcbFeign).getReviewConfiguration(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
        configurationStatusRequestListArgumentCaptor.capture());
    Assertions.assertEquals(CATEGORY_CODE, configurationStatusRequestListArgumentCaptor.getValue().get(0).getCategoryCode());
    Assertions.assertEquals(MERCHANT_CODE, configurationStatusRequestListArgumentCaptor.getValue().get(0).getBusinessPartnerCode());
  }

  @Test
  public void getReviewConfigurationExceptionTest() {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        new GdnRestListResponse<>(ERROR_MSG, ERROR_CODE, false, REQUEST_ID);
    Mockito.when(pcbFeign.getReviewConfiguration(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
        Mockito.anyList())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getReviewConfiguration(Collections.singletonList(
            ConfigurationStatusRequest.builder().categoryCode(CATEGORY_CODE).businessPartnerCode(MERCHANT_CODE).build()));
      });
    } finally {
      Mockito.verify(pcbFeign).getReviewConfiguration(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
          Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
          configurationStatusRequestListArgumentCaptor.capture());
      Assertions.assertEquals(CATEGORY_CODE, configurationStatusRequestListArgumentCaptor.getValue().get(0).getCategoryCode());
      Assertions.assertEquals(MERCHANT_CODE, configurationStatusRequestListArgumentCaptor.getValue().get(0).getBusinessPartnerCode());
    }
  }

  @Test
  public void getReviewConfigurationEmptyResultTest() {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        new GdnRestListResponse<>(null, new PageMetaData(10, 0, 10), REQUEST_ID);
    Mockito.when(pcbFeign.getReviewConfiguration(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
        Mockito.anyList())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getReviewConfiguration(Collections.singletonList(
            ConfigurationStatusRequest.builder().categoryCode(CATEGORY_CODE).businessPartnerCode(MERCHANT_CODE).build()));
      });
    } finally {
      Mockito.verify(pcbFeign).getReviewConfiguration(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
          Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
          configurationStatusRequestListArgumentCaptor.capture());
      Assertions.assertEquals(CATEGORY_CODE, configurationStatusRequestListArgumentCaptor.getValue().get(0).getCategoryCode());
      Assertions.assertEquals(MERCHANT_CODE, configurationStatusRequestListArgumentCaptor.getValue().get(0).getBusinessPartnerCode());
    }
  }

  @Test
  public void getCategoryDetailByCategoryCodeTest() {
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryCode(CATEGORY_CODE);
    GdnRestSingleResponse<CategoryDetailResponse> response =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    Mockito.when(pcbFeign.getCategoryDetailByCategoryCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(CATEGORY_CODE))).thenReturn(response);
    productOutboundBean.getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(pcbFeign).getCategoryDetailByCategoryCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(CATEGORY_CODE));
  }

  @Test
  public void getCategoryDetailByCategoryCodeTestExceptionTest() {
    GdnRestSingleResponse<CategoryDetailResponse> response =
        new GdnRestSingleResponse<>(ERROR_MSG, ERROR_CODE, false, null, REQUEST_ID);
    Mockito.when(pcbFeign.getCategoryDetailByCategoryCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(CATEGORY_CODE)))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getCategoryDetailByCategoryCode(CATEGORY_CODE);
      });
    } finally {
      Mockito.verify(pcbFeign).getCategoryDetailByCategoryCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
          Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(CATEGORY_CODE));
    }
  }

  @Test
  public void getCategoryBasicDetailByCategoryCodeTest() {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    GdnRestSingleResponse<CategoryResponse> response =
        new GdnRestSingleResponse<>(null, null, true, categoryResponse, REQUEST_ID);
    Mockito.when(pcbFeign.getCategoryBasicDetailByCategoryCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(CATEGORY_CODE))).thenReturn(response);
    productOutboundBean.getCategoryBasicDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(pcbFeign).getCategoryBasicDetailByCategoryCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(CATEGORY_CODE));
  }

  @Test
  public void getCategoryBasicDetailByCategoryCodeTestExceptionTest() {
    GdnRestSingleResponse<CategoryResponse> response =
        new GdnRestSingleResponse<>(ERROR_MSG, ERROR_CODE, false, null, REQUEST_ID);
    Mockito.when(pcbFeign.getCategoryBasicDetailByCategoryCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(CATEGORY_CODE)))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getCategoryBasicDetailByCategoryCode(CATEGORY_CODE);
      });
    } finally {
      Mockito.verify(pcbFeign).getCategoryBasicDetailByCategoryCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
          Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(CATEGORY_CODE));
    }
  }

  @Test
  public void getItemCodesByUpcCodeAndProductCode() {
    GdnRestListResponse<SingleObjectResponse<List<String>>> response = new GdnRestListResponse<>();
    Mockito.when(pcbFeign
        .getItemCodeByUpcCodeAndProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            any(ProductItemUpcCodesSkuCodesRequest.class))).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(new SingleObjectResponse<>(Arrays.asList(ITEM_NAME))),
            null, Constants.DEFAULT_REQUEST_ID));
    productOutboundBean.getItemCodesByUpcCodeAndProductCode(PRODUCT_CODE, new ProductItemUpcCodesSkuCodesRequest());
    Mockito.verify(pcbFeign)
        .getItemCodeByUpcCodeAndProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            any(ProductItemUpcCodesSkuCodesRequest.class));
  }

  @Test
  public void getItemCodesByUpcCodeAndProductCodeExceptionTest() {
    GdnRestListResponse<SingleObjectResponse<List<String>>> response = new GdnRestListResponse<>();
    Mockito.when(pcbFeign
        .getItemCodeByUpcCodeAndProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            any(ProductItemUpcCodesSkuCodesRequest.class))).thenReturn(
        new GdnRestListResponse<>(null, null, false, Arrays.asList(new SingleObjectResponse<>(Arrays.asList(ITEM_NAME))),
            null, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getItemCodesByUpcCodeAndProductCode(PRODUCT_CODE, new ProductItemUpcCodesSkuCodesRequest());
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getItemCodeByUpcCodeAndProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              any(ProductItemUpcCodesSkuCodesRequest.class));
    }
  }

  @Test
  public void getItemCodesByUpcCodeAndProductCodeSuccessFalseAndContentEmptyExceptionTest() {
    GdnRestListResponse<SingleObjectResponse<List<String>>> response = new GdnRestListResponse<>();
    Mockito.when(pcbFeign
        .getItemCodeByUpcCodeAndProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            any(ProductItemUpcCodesSkuCodesRequest.class))).thenReturn(
        new GdnRestListResponse<>(null, null, false, new ArrayList<>(),
            null, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getItemCodesByUpcCodeAndProductCode(PRODUCT_CODE, new ProductItemUpcCodesSkuCodesRequest());
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getItemCodeByUpcCodeAndProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              any(ProductItemUpcCodesSkuCodesRequest.class));
    }
  }

  @Test
  public void getAttributeDetailByAttributeCode() {
    List<AttributeResponse> attributeResponses = new ArrayList<>();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponses.add(attributeResponse);
    GdnRestListResponse<AttributeResponse> response =
        new GdnRestListResponse<>(attributeResponses, new PageMetaData(0, 0, 10), REQUEST_ID);
    Mockito.when(pcbFeign.getAttributeDetailByAttributeCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(ATTRIBUTE_CODE))).thenReturn(response);
    productOutboundBean.getAttributeDetailByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(pcbFeign).getAttributeDetailByAttributeCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(ATTRIBUTE_CODE));
  }

  @Test
  public void getAttributeDetailByAttributeCodeExceptionTest() {
    GdnRestListResponse<AttributeResponse> response =
        new GdnRestListResponse<>(ERROR_MSG, ERROR_CODE, false, REQUEST_ID);
    Mockito.when(pcbFeign.getAttributeDetailByAttributeCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(ATTRIBUTE_CODE)))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getAttributeDetailByAttributeCode(ATTRIBUTE_CODE);
      });
    } finally {
      Mockito.verify(pcbFeign).getAttributeDetailByAttributeCode(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
          Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(), Mockito.eq(ATTRIBUTE_CODE));
    }
  }

  @Test
  public void getRestrictedKeywordMappedToCategoryTest() {
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse.setKeyword(KEYWORD);
    GdnRestListResponse<RestrictedKeywordsMappedToCategoryResponse> gdnResponse =
        new GdnRestListResponse<>(Arrays.asList(restrictedKeywordsMappedToCategoryResponse), new PageMetaData(0, 0, 10),
            REQUEST_ID);
    Mockito.when(
        pcbFeign.getRestrictedKeywordMappedToCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
            Mockito.eq(CATEGORY_CODE))).thenReturn(gdnResponse);
    this.productOutboundBean.getRestrictedKeywordMappedToCategory(CATEGORY_CODE);
    Mockito.verify(pcbFeign)
        .getRestrictedKeywordMappedToCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
            Mockito.eq(CATEGORY_CODE));
  }

  @Test
  public void getRestrictedKeywordMappedToCategoryExceptionTest() {
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse.setKeyword(KEYWORD);
    GdnRestListResponse<RestrictedKeywordsMappedToCategoryResponse> gdnResponse =
        new GdnRestListResponse<>(null, null, false, Arrays.asList(restrictedKeywordsMappedToCategoryResponse),
            new PageMetaData(0, 0, 0), REQUEST_ID);
    Mockito.when(
        pcbFeign.getRestrictedKeywordMappedToCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
            Mockito.eq(CATEGORY_CODE))).thenReturn(gdnResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productOutboundBean.getRestrictedKeywordMappedToCategory(CATEGORY_CODE);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getRestrictedKeywordMappedToCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
              Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
              Mockito.eq(CATEGORY_CODE));
    }
  }


  @Test
  public void getRestrictedKeywordMappedToCategoryWithActionTest() {
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse.setKeyword(KEYWORD);
    GdnRestListResponse<RestrictedKeywordsMappedToCategoryResponse> gdnResponse =
        new GdnRestListResponse<>(Arrays.asList(restrictedKeywordsMappedToCategoryResponse), new PageMetaData(0, 0, 10),
            REQUEST_ID);
    Mockito.when(
        pcbFeign.getRestrictedKeywordMappedToCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
            Mockito.eq(CATEGORY_CODE))).thenReturn(gdnResponse);
    this.productOutboundBean.getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE);
    Mockito.verify(pcbFeign)
        .getRestrictedKeywordMappedToCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
            Mockito.eq(CATEGORY_CODE));
  }

  @Test
  public void getRestrictedKeywordMappedToCategoryWithActionExceptionTest() {
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse.setKeyword(KEYWORD);
    GdnRestListResponse<RestrictedKeywordsMappedToCategoryResponse> gdnResponse =
        new GdnRestListResponse<>(null, null, false, Arrays.asList(restrictedKeywordsMappedToCategoryResponse),
            new PageMetaData(0, 0, 0), REQUEST_ID);
    Mockito.when(
        pcbFeign.getRestrictedKeywordMappedToCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
            Mockito.eq(CATEGORY_CODE))).thenReturn(gdnResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productOutboundBean.getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getRestrictedKeywordMappedToCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
              Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
              Mockito.eq(CATEGORY_CODE));
    }
  }

  @Test
  public void getRestrictedKeywordMappedToCategoryEmptyListTest() {
    GdnRestListResponse<RestrictedKeywordsMappedToCategoryResponse> gdnResponse =
        new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(0, 0, 10), REQUEST_ID);
    Mockito.when(
        pcbFeign.getRestrictedKeywordMappedToCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
            Mockito.eq(CATEGORY_CODE))).thenReturn(gdnResponse);
    this.productOutboundBean.getRestrictedKeywordMappedToCategory(CATEGORY_CODE);
    Mockito.verify(pcbFeign)
        .getRestrictedKeywordMappedToCategory(any(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), any(), any(),
            Mockito.eq(CATEGORY_CODE));
  }

  @Test
  public void clearProductCacheByProductCodesTest() throws Exception {
    Mockito.when(pcbFeign.clearProductCacheByProductCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        Arrays.asList(PRODUCT_CODE))).thenReturn(new GdnBaseRestResponse(true));
    this.productOutboundBean.clearProductCacheByProductCodes(Arrays.asList(PRODUCT_CODE));
    Mockito.verify(pcbFeign).clearProductCacheByProductCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void clearProductCacheByProductCodesExceptionTest() throws Exception {
    Mockito.when(pcbFeign.clearProductCacheByProductCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        Arrays.asList(PRODUCT_CODE))).thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productOutboundBean.clearProductCacheByProductCodes(Arrays.asList(PRODUCT_CODE));
      });
    } finally {
      Mockito.verify(pcbFeign).clearProductCacheByProductCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          Arrays.asList(PRODUCT_CODE));
    }
  }

  @Test
  public void clearProductCacheSyncByProductIdAndProductCodeTest() {
    Mockito.when(pcbFeign
        .clearProductCacheSyncByProductIdAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID,
            PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(true));
    this.productOutboundBean.clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE);
    Mockito.verify(pcbFeign)
        .clearProductCacheSyncByProductIdAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID,
            PRODUCT_CODE);
  }

  @Test
  public void clearProductCacheSyncByProductIdAndProductCodeExceptionTest() {
    Mockito.when(pcbFeign
        .clearProductCacheSyncByProductIdAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID,
            PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productOutboundBean.clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .clearProductCacheSyncByProductIdAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID,
              PRODUCT_CODE);
    }
  }

  @Test
  public void updateProductImagesNameTest() throws Exception {
    GdnRestSingleResponse<ActivateImageResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, activateImageResponse, REQUEST_ID);
    Mockito.when(pcbFeign
        .updateProductImagesName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, productActivateImageRequest))
        .thenReturn(gdnRestSingleResponse);
    productOutboundBean.updateProductImagesName(productActivateImageRequest, true);
    Mockito.verify(pcbFeign)
        .updateProductImagesName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, productActivateImageRequest);
  }

  @Test
  public void updateProductImagesNameExceptionTest() throws Exception {
    GdnRestSingleResponse<ActivateImageResponse> response =
        new GdnRestSingleResponse<>(ERROR_MSG, ERROR_CODE, false, null, REQUEST_ID);
    Mockito.when(pcbFeign
        .updateProductImagesName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, productActivateImageRequest))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateProductImagesName(productActivateImageRequest, false);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .updateProductImagesName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, productActivateImageRequest);
    }
  }

  @Test
  public void migrateProductTest() throws Exception {
    GdnRestSimpleResponse<String> gdnRestSimpleResponse =
        new GdnRestSimpleResponse<>(null, null, true, REQUEST_ID, PRODUCT_ID);
    Mockito.when(pcbFeign
        .migrateProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, OLD_PRODUCT_CODE, PRODUCT_CODE, MERCHANT_CODE)).thenReturn(gdnRestSimpleResponse);
    String value = productOutboundBean.migrateProduct(OLD_PRODUCT_CODE, PRODUCT_CODE, MERCHANT_CODE, null);
    Mockito.verify(pcbFeign)
        .migrateProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, OLD_PRODUCT_CODE, PRODUCT_CODE, MERCHANT_CODE);
    Assertions.assertEquals(PRODUCT_ID, value);
  }

  @Test
  public void migrateProductExceptionTest() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    GdnRestSimpleResponse<String> gdnRestSimpleResponse =
        new GdnRestSimpleResponse<>(ERROR_MSG, ERROR_CODE, false, null, REQUEST_ID);
    Mockito.when(pcbFeign
        .migrateProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, OLD_PRODUCT_CODE, PRODUCT_CODE, MERCHANT_CODE,
            productRequest)).thenReturn(gdnRestSimpleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.migrateProduct(OLD_PRODUCT_CODE, PRODUCT_CODE, MERCHANT_CODE, productRequest);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .migrateProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, OLD_PRODUCT_CODE, PRODUCT_CODE, MERCHANT_CODE,
              productRequest);
    }
  }


  @Test
  public void updateProductContentTest() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse();
    gdnBaseRestResponse.setSuccess(true);
    Mockito.when(pcbFeign
        .updateProductContent(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, productRequest)).thenReturn(gdnBaseRestResponse);
    productOutboundBean.updateProductContent(productRequest, false);
    Mockito.verify(pcbFeign)
        .updateProductContent(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, productRequest);
  }

  @Test
  public void getProductItemBySkuCodesTest() throws Exception {
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setFetchArchived(false);
    skuCodesRequest.setSkuCodes(Arrays.asList(Constants.SKU_CODE));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(Constants.SKU_CODE);
    GdnRestListResponse<ProductItemResponse> response =
        new GdnRestListResponse<>(Arrays.asList(productItemResponse), new PageMetaData(), REQUEST_ID);
    Mockito.when(pcbFeign
        .getProductItemBySkuCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, skuCodesRequest)).thenReturn(response);
    List<ProductItemResponse> responses = productOutboundBean.getProductItemBySkuCodes(skuCodesRequest);
    Mockito.verify(pcbFeign)
        .getProductItemBySkuCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, skuCodesRequest);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertEquals(Constants.SKU_CODE, responses.get(0).getSkuCode());
  }

  @Test
  public void getProductItemBySkuCodesExceptionTest() throws Exception {
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setFetchArchived(false);
    skuCodesRequest.setSkuCodes(Arrays.asList(Constants.SKU_CODE));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(Constants.SKU_CODE);
    GdnRestListResponse<ProductItemResponse> response =
        new GdnRestListResponse<>(ERROR_MSG, ERROR_CODE, false, REQUEST_ID);
    Mockito.when(pcbFeign
        .getProductItemBySkuCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, skuCodesRequest)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getProductItemBySkuCodes(skuCodesRequest);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getProductItemBySkuCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, skuCodesRequest);
      Assertions.assertFalse(response.isSuccess());
    }
  }

  @Test
  public void editItemUpcCodeTest() {
    ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequest =
        ProductItemUpcCodeUpdateRequest.builder().upcCode(Constants.UPC_CODE).skuCode(Constants.SKU_CODE).build();
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    Mockito.when(pcbFeign
        .editItemUpcCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,
            Arrays.asList(productItemUpcCodeUpdateRequest))).thenReturn(response);
    productOutboundBean.editItemUpcCode(PRODUCT_CODE, Arrays.asList(productItemUpcCodeUpdateRequest));
    Mockito.verify(pcbFeign)
        .editItemUpcCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,
            Arrays.asList(productItemUpcCodeUpdateRequest));
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void editItemUpcCodeExceptionTest() {
    ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequest =
        ProductItemUpcCodeUpdateRequest.builder().upcCode(Constants.UPC_CODE).skuCode(Constants.SKU_CODE).build();
    GdnBaseRestResponse response = new GdnBaseRestResponse(false);
    Mockito.when(pcbFeign
        .editItemUpcCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,
            Arrays.asList(productItemUpcCodeUpdateRequest))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.editItemUpcCode(PRODUCT_CODE, Arrays.asList(productItemUpcCodeUpdateRequest));
      });
    } finally {
      Mockito.verify(pcbFeign)
          .editItemUpcCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,
              Arrays.asList(productItemUpcCodeUpdateRequest));
      Assertions.assertFalse(response.isSuccess());
    }
  }

  @Test
  public void updateProductItemImagesByProductCodeTest() {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    GdnRestListResponse<LocationPathAndCommonImage> response =
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 0, 0), REQUEST_ID);
    Mockito.when(pcbFeign.updateProductItemImagesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        productItemImageUpdateRequest)).thenReturn(response);
    productOutboundBean.updateProductItemImagesByProductCode(productItemImageUpdateRequest);
    Mockito.verify(pcbFeign)
        .updateProductItemImagesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productItemImageUpdateRequest);
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void updateProductItemImagesByProductCodeExceptionTest() {
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    GdnRestListResponse<LocationPathAndCommonImage> response =
        new GdnRestListResponse<>(null, null, false, new ArrayList<>(), new PageMetaData(0, 0, 0), REQUEST_ID);
    Mockito.when(pcbFeign.updateProductItemImagesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        productItemImageUpdateRequest)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateProductItemImagesByProductCode(productItemImageUpdateRequest);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .updateProductItemImagesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
              productItemImageUpdateRequest);
      Assertions.assertFalse(response.isSuccess());
    }
  }

  @Test
  public void updateProductAndItemImagesByProductCodeTest() throws Exception {
    ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse();
    gdnBaseRestResponse.setSuccess(true);
    Mockito.when(pcbFeign
        .updateProductAndItemImagesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, productAndItemImageRequest)).thenReturn(gdnBaseRestResponse);
    productOutboundBean.updateProductAndItemImagesByProductCode(false, productAndItemImageRequest);
    Mockito.verify(pcbFeign)
        .updateProductAndItemImagesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, productAndItemImageRequest);
  }

  @Test
  public void findAllowedAttributeValueTest() throws Exception {
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    allowedAttributeValueRequest.setAttributeCode(ATTRIBUTE_CODE);
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAttributeCode(ATTRIBUTE_CODE);
    GdnRestListResponse<AllowedAttributeValueResponse> gdnRestListResponse1 = new GdnRestListResponse<>();
    gdnRestListResponse1.setSuccess(true);
    gdnRestListResponse1.setContent(Collections.singletonList(allowedAttributeValueResponse));
    Mockito.when(pcbFeign.findAllowedAttributeValue(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), any())).thenReturn(gdnRestListResponse1);
    List<AllowedAttributeValueResponse> response =
        productOutboundBean.findAllowedAttributeValue(Collections.singletonList(allowedAttributeValueRequest));
    Mockito.verify(pcbFeign)
        .findAllowedAttributeValue(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), any());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.get(0).getAttributeCode());
  }

  @Test
  public void findAllowedAttributeValueSuccessFalseTest() throws Exception {
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    allowedAttributeValueRequest.setAttributeCode(ATTRIBUTE_CODE);
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAttributeCode(ATTRIBUTE_CODE);
    GdnRestListResponse<AllowedAttributeValueResponse> gdnRestListResponse1 = new GdnRestListResponse<>();
    gdnRestListResponse1.setSuccess(false);
    gdnRestListResponse1.setContent(Collections.singletonList(allowedAttributeValueResponse));
    Mockito.when(pcbFeign.findAllowedAttributeValue(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), any())).thenReturn(gdnRestListResponse1);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        List<AllowedAttributeValueResponse> response =
            productOutboundBean.findAllowedAttributeValue(Collections.singletonList(allowedAttributeValueRequest));
      });
    } finally {
      Mockito.verify(pcbFeign)
          .findAllowedAttributeValue(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), any());
    }
  }

  @Test
  public void findAllowedAttributeValueContentNullTest() throws Exception {
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    allowedAttributeValueRequest.setAttributeCode(ATTRIBUTE_CODE);
    GdnRestListResponse<AllowedAttributeValueResponse> gdnRestListResponse1 = new GdnRestListResponse<>();
    gdnRestListResponse1.setSuccess(true);
    gdnRestListResponse1.setContent(null);
    Mockito.when(pcbFeign.findAllowedAttributeValue(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), any())).thenReturn(gdnRestListResponse1);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        List<AllowedAttributeValueResponse> response =
            productOutboundBean.findAllowedAttributeValue(Collections.singletonList(allowedAttributeValueRequest));
      });
    } finally {
      Mockito.verify(pcbFeign)
          .findAllowedAttributeValue(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), any());
    }
  }


  @Test
  public void getPredefinedAllowedAttributeValueByAttributeCodeAndValueTest() {
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue(ATTRIBUTE_CODE);
    GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>();
    gdnRestSingleResponse.setSuccess(true);
    gdnRestSingleResponse.setValue(predefinedAllowedAttributeValueResponse);
    Mockito.when(pcbFeign.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, ATTRIBUTE_CODE, ATTRIBUTE_CODE, false)).thenReturn(gdnRestSingleResponse);
    PredefinedAllowedAttributeValueResponse response =
        productOutboundBean.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(ATTRIBUTE_CODE, ATTRIBUTE_CODE,
            false);
    Mockito.verify(pcbFeign).getPredefinedAllowedAttributeValueByAttributeCodeAndValue(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, ATTRIBUTE_CODE, ATTRIBUTE_CODE, false);
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getValue());
  }

  @Test
  public void getPredefinedAllowedAttributeValueByAttributeCodeAndValueWithSuccessFalseTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(pcbFeign.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, ATTRIBUTE_CODE, ATTRIBUTE_CODE, false)).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(ATTRIBUTE_CODE, ATTRIBUTE_CODE,
            false);
      });
    } finally {
      Mockito.verify(pcbFeign).getPredefinedAllowedAttributeValueByAttributeCodeAndValue(Constants.DEFAULT_STORE_ID,
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
          Constants.DEFAULT_USERNAME, ATTRIBUTE_CODE, ATTRIBUTE_CODE, false);
    }
  }

  @Test
  public void filterProductImagesByProductIdsTest() {
    ProductImageResponse productImageResponse = new ProductImageResponse();
    productImageResponse.setLocationPath(LOCATION_PATH);
    gdnRestSingleResponse.setSuccess(true);
    gdnRestSingleResponse.setContent(Arrays.asList(productImageResponse));
    Mockito.when(pcbFeign.filterProductImagesByProductIds(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any(),
        Mockito.anyBoolean())).thenReturn(gdnRestSingleResponse);
    List<ProductImageResponse> response = productOutboundBean.filterProductImagesByProductIds(productIds, true);
    Mockito.verify(pcbFeign).filterProductImagesByProductIds(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any(),
        Mockito.anyBoolean());
    Assertions.assertEquals(LOCATION_PATH, response.get(0).getLocationPath());
  }

  @Test
  public void filterProductImagesByProductIdsNullTest() {
    ProductImageResponse productImageResponse = new ProductImageResponse();
    productImageResponse.setLocationPath(LOCATION_PATH);
    gdnRestSingleResponse.setSuccess(true);
    gdnRestSingleResponse.setContent(null);
    Mockito.when(pcbFeign.filterProductImagesByProductIds(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any(),
        Mockito.anyBoolean())).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.filterProductImagesByProductIds(productIds, true);
      });
    } finally {
      Mockito.verify(pcbFeign).filterProductImagesByProductIds(Mockito.eq(Constants.DEFAULT_STORE_ID),
          Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
          Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any(),
          Mockito.anyBoolean());
    }
  }

  @Test
  public void filterProductImagesByProductIdsFalseTest() {
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(pcbFeign.filterProductImagesByProductIds(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any(),
        Mockito.anyBoolean())).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.filterProductImagesByProductIds(productIds, true);
      });
    } finally {
      Mockito.verify(pcbFeign).filterProductImagesByProductIds(Mockito.eq(Constants.DEFAULT_STORE_ID),
          Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
          Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any(),
          Mockito.anyBoolean());
    }
  }

  @Test
  public void filterProductImagesByProductIdsFalseContentNullTest() {
    gdnRestSingleResponse.setSuccess(false);
    gdnRestSingleResponse.setContent(null);
    Mockito.when(pcbFeign.filterProductImagesByProductIds(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any(),
        Mockito.anyBoolean())).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.filterProductImagesByProductIds(productIds, true);
      });
    } finally {
      Mockito.verify(pcbFeign).filterProductImagesByProductIds(Mockito.eq(Constants.DEFAULT_STORE_ID),
          Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
          Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any(),
          Mockito.anyBoolean());
    }
  }

  @Test
  public void validCnCategoryTest() {
    GdnRestSingleResponse<CategoryResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>();
    gdnRestSingleResponse.setSuccess(true);
    gdnRestSingleResponse.setErrorMessage(null);
    Mockito.when(pcbFeign.validateCnCategory(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE)))
        .thenReturn(gdnRestSingleResponse);
    productOutboundBean.validCnCategory(CATEGORY_CODE);
    Mockito.verify(pcbFeign).validateCnCategory(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE));
  }

  @Test
  public void validCnCategorySuccessTrueWithErrorCodeTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(true);
    gdnRestSingleResponse.setErrorMessage(ERROR_CODE);
    Mockito.when(pcbFeign.validateCnCategory(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE)))
        .thenReturn(gdnRestSingleResponse);
    productOutboundBean.validCnCategory(CATEGORY_CODE);
    Mockito.verify(pcbFeign).validateCnCategory(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE));
  }

  @Test
  public void validCnCategorySuccessFalse() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(false);
    gdnRestSingleResponse.setErrorMessage(null);
    Mockito.when(pcbFeign.validateCnCategory(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE)))
        .thenReturn(gdnRestSingleResponse);
    productOutboundBean.validCnCategory(CATEGORY_CODE);
    Mockito.verify(pcbFeign).validateCnCategory(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE));
  }

  @Test
  public void validCnCategorySuccessFalseWithErrorCode() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(false);
    gdnRestSingleResponse.setErrorMessage(ERROR_MSG);
    Mockito.when(pcbFeign.validateCnCategory(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE)))
        .thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.validCnCategory(CATEGORY_CODE);
      });
    }finally {
      Mockito.verify(pcbFeign).validateCnCategory(Mockito.eq(Constants.DEFAULT_STORE_ID),
          Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
          Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE));
    }
  }

  @Test
  public void getProductDetailByProductCodeTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(true);
    gdnRestSingleResponse.setValue(new ProductDetailResponse());
    Mockito.when(pcbFeign
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
            PRODUCT_CODE, false)).thenReturn(gdnRestSingleResponse);
    productOutboundBean.getProductDetailByProductCode(PRODUCT_CODE, true, false);
    Mockito.verify(pcbFeign)
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
            PRODUCT_CODE, false);
  }

  @Test
  public void getProductDetailByProductCodeExceptionTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(pcbFeign
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
            PRODUCT_CODE, false)).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getProductDetailByProductCode(PRODUCT_CODE, true, false);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
              PRODUCT_CODE, false);
    }
  }

  @Test
  public void getProductDetailByProductCodeEmptyResponseExceptionTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(true);
    Mockito.when(pcbFeign
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
            PRODUCT_CODE, false)).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getProductDetailByProductCode(PRODUCT_CODE, true, false);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
              PRODUCT_CODE, false);
    }
  }

  @Test
  public void updateFlagsOnNeedCorrectionTest() throws Exception {
    NeedRevisionConfigRequest needRevisionConfigRequest = new NeedRevisionConfigRequest();
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse();
    gdnBaseRestResponse.setSuccess(true);
    Mockito.when(pcbFeign.updateFlagsOnNeedCorrection(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,
        needRevisionConfigRequest)).thenReturn(gdnBaseRestResponse);
    productOutboundBean.updateFlagsOnNeedCorrection(PRODUCT_CODE, needRevisionConfigRequest);
    Mockito.verify(pcbFeign).updateFlagsOnNeedCorrection(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,
        needRevisionConfigRequest);
  }

  @Test
  public void updateFlagsOnNeedCorrectionExceptionTest() throws Exception {
    NeedRevisionConfigRequest needRevisionConfigRequest = new NeedRevisionConfigRequest();
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse();
    Mockito.when(pcbFeign.updateFlagsOnNeedCorrection(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,
        needRevisionConfigRequest)).thenReturn(gdnBaseRestResponse);
    gdnBaseRestResponse.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateFlagsOnNeedCorrection(PRODUCT_CODE, needRevisionConfigRequest);
      });
    } finally {
      Mockito.verify(pcbFeign).updateFlagsOnNeedCorrection(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,
          needRevisionConfigRequest);
    }
  }

  @Test
  public void getImagesForScalingByProductCodeTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(true);
    gdnRestSingleResponse.setValue(new ProductDetailResponse());
    Mockito.when(pcbFeign.getImagesForScalingByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(gdnRestSingleResponse);
    productOutboundBean.getImagesForScalingByProductCode(PRODUCT_CODE);
    Mockito.verify(pcbFeign).getImagesForScalingByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
  }

  @Test
  public void getImagesForScalingByProductCodeExceptionTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(pcbFeign.getImagesForScalingByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getImagesForScalingByProductCode(PRODUCT_CODE);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getImagesForScalingByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    }
  }

  @Test
  public void getImagesForScalingByProductCodeEmptyResponseExceptionTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(true);
    Mockito.when(pcbFeign.getImagesForScalingByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getImagesForScalingByProductCode(PRODUCT_CODE);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getImagesForScalingByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    }
  }

  @Test
  public void getProductItemIdsBySkuCodesTest() throws Exception {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setValue(new SingleObjectResponse<>(new HashMap<>()));
    gdnRestSingleResponse.setSuccess(true);
    gdnRestSingleResponse.setErrorMessage(null);
    Mockito.when(pcbFeign.getProductItemIdsBySkuCode(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any()))
        .thenReturn(gdnRestSingleResponse);
    productOutboundBean.getProductItemIdsBySkuCodes(new ArrayList<>());
    Mockito.verify(pcbFeign).getProductItemIdsBySkuCode(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any());
  }

  @Test
  public void getProductItemIdsBySkuCodesExceptionTest() throws Exception {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setValue(new SingleObjectResponse<>(new HashMap<>()));
    gdnRestSingleResponse.setSuccess(false);
    gdnRestSingleResponse.setErrorMessage(null);
    Mockito.when(pcbFeign.getProductItemIdsBySkuCode(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any()))
        .thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getProductItemIdsBySkuCodes(new ArrayList<>());
      });
    } finally {
      Mockito.verify(pcbFeign).getProductItemIdsBySkuCode(Mockito.eq(Constants.DEFAULT_STORE_ID),
          Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
          Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), any());
    }
  }

  @Test
  public void updateAndMarkProductForNeedCorrectionTest() {
    Mockito.when(pcbFeign
        .updateAndMarkProductForNeedCorrection(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productRequest.getProductCode(), productRequest))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    productOutboundBean.updateAndMarkProductForNeedCorrection(productRequest);
    Mockito.verify(pcbFeign)
        .updateAndMarkProductForNeedCorrection(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productRequest.getProductCode(), productRequest);
  }

  @Test
  public void updateAndMarkProductForNeedCorrection_successFalseTest() {
    Mockito.when(pcbFeign
        .updateAndMarkProductForNeedCorrection(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productRequest.getProductCode(), productRequest))
        .thenReturn(new GdnBaseRestResponse(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateAndMarkProductForNeedCorrection(productRequest);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .updateAndMarkProductForNeedCorrection(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
              productRequest.getProductCode(), productRequest);
    }
  }

  @Test
  public void filterCategoryHierarchyByCategoryCodeTest() throws Exception {
    GdnRestListResponse gdnRestListResponse =
        new GdnRestListResponse(Arrays.asList(), new PageMetaData(0, 0, 100), REQUEST_ID);
    Mockito.when(pcbFeign.filterCategoryHierarchyByCategoryCode(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE)))
        .thenReturn(gdnRestListResponse);
    productOutboundBean.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE);
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE));
  }

  @Test
  public void filterCategoryHierarchyByCategoryCodeExceptionTest() throws Exception {
    GdnRestListResponse gdnRestListResponse = new GdnRestListResponse(null, new PageMetaData(0, 0, 100), REQUEST_ID);
    gdnRestListResponse.setSuccess(false);
    Mockito.when(pcbFeign.filterCategoryHierarchyByCategoryCode(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE)))
        .thenReturn(gdnRestListResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE);
      });
    } finally {
      Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(Mockito.eq(Constants.DEFAULT_STORE_ID),
          Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
          Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(CATEGORY_CODE));
    }
  }

  @Test
  public void deleteOriginalImagesTest() {
    Mockito.when(pcbFeign.deleteOriginalImages(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(new GdnBaseRestResponse(true));
    productOutboundBean.deleteOriginalImages(PRODUCT_CODE);
    Mockito.verify(pcbFeign).deleteOriginalImages(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_CODE));
  }

  @Test
  public void deleteOriginalImagesExceptionTest() {
    Mockito.when(pcbFeign.deleteOriginalImages(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.deleteOriginalImages(PRODUCT_CODE);
      });
    } finally {
      Mockito.verify(pcbFeign).deleteOriginalImages(Mockito.eq(Constants.DEFAULT_STORE_ID),
          Mockito.eq(Constants.DEFAULT_CHANNEL_ID), Mockito.eq(Constants.DEFAULT_CLIENT_ID),
          Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_CODE));
    }
  }

  @Test
  public void updateImagesTest() {
    Mockito.when(pcbFeign.updateImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(productImageEditRequest)))
        .thenReturn(new GdnRestSingleResponse(new SingleObjectResponse(), Constants.DEFAULT_REQUEST_ID));
    productOutboundBean.updateImages(productImageEditRequest);
    Mockito.verify(pcbFeign)
        .updateImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(productImageEditRequest));
  }

  @Test
  public void updateImagesExceptionTest() {
    Mockito.when(pcbFeign.updateImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(productImageEditRequest)))
        .thenReturn(new GdnRestSingleResponse(null, null, false, null, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateImages(productImageEditRequest);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .updateImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
              Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
              Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(productImageEditRequest));
    }
  }

  @Test
  public void updateImagesException1Test() {
    Mockito.when(pcbFeign.updateImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(productImageEditRequest)))
        .thenReturn(new GdnRestSingleResponse(null, null, true, null, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateImages(productImageEditRequest);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .updateImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
              Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
              Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(productImageEditRequest));
    }
  }

  @Test
  public void authoriseProtectedBrandTest(){
    Mockito.when(pcbFeign
      .getBrandAuthorisation(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        BRAND_CODE, BUSINESS_PARTNER_CODE)).thenReturn(
      new GdnRestSingleResponse(null, null, true, new SimpleBooleanResponse(),
        Constants.DEFAULT_REQUEST_ID));
    productOutboundBean
      .authoriseProtectedBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        BRAND_CODE, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbFeign)
      .getBrandAuthorisation(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        BRAND_CODE, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void authoriseProtectedBrandExceptionTest() {
    Mockito.when(pcbFeign
      .getBrandAuthorisation(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        BRAND_CODE, BUSINESS_PARTNER_CODE)).thenReturn(
      new GdnRestSingleResponse(null, null, false, new SimpleBooleanResponse(),
        Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.authoriseProtectedBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE,
            BUSINESS_PARTNER_CODE);
      });
    } finally {
      Mockito.verify(pcbFeign)
        .getBrandAuthorisation(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          BRAND_CODE, BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void findProductNameByProductItemIdTest() {
    Mockito.when(pcbFeign.getProductNameByProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
        Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_ITEM_ID)))
        .thenReturn(new SingleBaseResponse<>(null, null, true, REQUEST_ID, ITEM_NAME));
    String itemName = productOutboundBean.findProductNameByProductItemId(PRODUCT_ITEM_ID);
    Mockito.verify(pcbFeign)
        .getProductNameByProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_ITEM_ID));
    Assertions.assertEquals(itemName, ITEM_NAME);
  }

  @Test
  public void findProductNameByProductItemIdSuccessFalseTest() {
    Mockito.when(pcbFeign.getProductNameByProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
        Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_ITEM_ID)))
        .thenReturn(new SingleBaseResponse<>(null, null, false, REQUEST_ID, ITEM_NAME));
    String itemName = productOutboundBean.findProductNameByProductItemId(PRODUCT_ITEM_ID);
    Mockito.verify(pcbFeign)
        .getProductNameByProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_ITEM_ID));
    Assertions.assertEquals(Constants.DEFAULT, itemName);
  }

  @Test
  public void findProductNameByProductItemIdItemNameNullTest() {
    Mockito.when(pcbFeign.getProductNameByProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
        Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_ITEM_ID)))
        .thenReturn(new SingleBaseResponse<>(null, null, true, REQUEST_ID, null));
    String itemName = productOutboundBean.findProductNameByProductItemId(PRODUCT_ITEM_ID);
    Mockito.verify(pcbFeign)
        .getProductNameByProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_ITEM_ID));
    Assertions.assertEquals(Constants.DEFAULT, itemName);
  }

  @Test
  public void findProductNameByProductItemIdSuccessFalseAndItemNameNullTest() {
    Mockito.when(pcbFeign.getProductNameByProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
        Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_ITEM_ID)))
        .thenReturn(new SingleBaseResponse<>(null, null, false, REQUEST_ID, null));
    String itemName = productOutboundBean.findProductNameByProductItemId(PRODUCT_ITEM_ID);
    Mockito.verify(pcbFeign)
        .getProductNameByProductItemId(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_ITEM_ID));
    Assertions.assertEquals(Constants.DEFAULT, itemName);
  }

  @Test
  public void fetchCategoryNamesResponseTest(){
    Mockito.when(pcbFeign
        .getCategoryNames(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            PAGE, SIZE, categoryMultipleIdRequest)).thenReturn(
        new GdnRestSingleResponse(null, null, true, new CategoryNamesResponse(),
            Constants.DEFAULT_REQUEST_ID));
    productOutboundBean.fetchCategoryNamesResponse(categoryMultipleIdRequest, PAGE, SIZE);
    Mockito.verify(pcbFeign)
        .getCategoryNames(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            PAGE, SIZE, categoryMultipleIdRequest);
  }

  @Test
  public void fetchCategoryNamesResponseExceptionTest() {
    Mockito.when(pcbFeign
        .getCategoryNames(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            PAGE, SIZE, categoryMultipleIdRequest)).thenReturn(
        new GdnRestSingleResponse(null, null, false, new CategoryNamesResponse(),
            Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.fetchCategoryNamesResponse(categoryMultipleIdRequest, PAGE, SIZE);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getCategoryNames(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
              PAGE, SIZE, categoryMultipleIdRequest);
    }
  }

  @Test
  public void updateCommonImagesTest() {
    Mockito.when(pcbFeign.updateCommonImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(Arrays.asList(productImageEditRequest))))
        .thenReturn(new GdnRestSingleResponse(new SingleObjectResponse(), Constants.DEFAULT_REQUEST_ID));
    productOutboundBean.updateCommonImages(Arrays.asList(productImageEditRequest));
    Mockito.verify(pcbFeign)
        .updateCommonImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(Arrays.asList(productImageEditRequest)));
  }

  @Test
  public void updateCommonImagesExceptionTest() {
    Mockito.when(pcbFeign.updateCommonImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
            Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(Arrays.asList(productImageEditRequest))))
        .thenReturn(new GdnRestSingleResponse(null, null, false, null, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateCommonImages(Arrays.asList(productImageEditRequest));
      });
    } finally {
      Mockito.verify(pcbFeign)
          .updateCommonImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
              Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
              Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(Arrays.asList(productImageEditRequest)));
    }
  }

  @Test
  public void updateCommonImagesException1Test() {
    Mockito.when(
            pcbFeign.updateCommonImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
                Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
                Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(Arrays.asList(productImageEditRequest))))
        .thenReturn(new GdnRestSingleResponse(null, null, true, null, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateCommonImages(Arrays.asList(productImageEditRequest));
      });
    } finally {
      Mockito.verify(pcbFeign)
          .updateCommonImages(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
              Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.eq(Constants.DEFAULT_REQUEST_ID),
              Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(Arrays.asList(productImageEditRequest)));
    }
  }

  @Test
  public void getPredictionIdAndCategoryCodeTest() throws Exception {
    PredictionIdsRequest predictionIdsRequest = new PredictionIdsRequest();
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(PREDICTION_ID);
    predictionIdsRequest.setPredictionIdList(predictionIdList);
    List<PredictionIdAndCategoryCodeResponse> responseList = new ArrayList<>();
    Mockito.when(pcbFeign.getPredictionIdAndCategoryCode(any(), Mockito.anyString(), Mockito.anyString(),
            any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, responseList, null, REQUEST_ID));
    try {
      productOutboundBean.getPredictionIdAndCategoryCode(predictionIdsRequest);
    } finally {
      Mockito.verify(pcbFeign)
          .getPredictionIdAndCategoryCode(any(), Mockito.anyString(), Mockito.anyString(),
              any(), any(), any());
    }
  }

  @Test
  public void getPredictionIdAndCategoryCodeApplicationRuntimeExceptionTest() throws Exception {
    PredictionIdsRequest predictionIdsRequest = new PredictionIdsRequest();
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(PREDICTION_ID);
    predictionIdsRequest.setPredictionIdList(predictionIdList);
    List<PredictionIdAndCategoryCodeResponse> responseList = new ArrayList<>();
    Mockito.when(pcbFeign.getPredictionIdAndCategoryCode(any(), Mockito.anyString(), Mockito.anyString(),
        any(), any(), any())).thenReturn(
        new GdnRestListResponse<>(null, ErrorCategory.VALIDATION.getCode(), false, responseList, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getPredictionIdAndCategoryCode(predictionIdsRequest);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getPredictionIdAndCategoryCode(any(), Mockito.anyString(), Mockito.anyString(),
              any(), any(), any());
    }
  }

  @Test
  public void getPredictionIdAndCategoryCodeExceptionTest() throws Exception {
    PredictionIdsRequest predictionIdsRequest = new PredictionIdsRequest();
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(PREDICTION_ID);
    predictionIdsRequest.setPredictionIdList(predictionIdList);
    List<PredictionIdAndCategoryCodeResponse> responseList = new ArrayList<>();
    Mockito.when(pcbFeign.getPredictionIdAndCategoryCode(any(), Mockito.anyString(), Mockito.anyString(),
            any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, false, responseList, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getPredictionIdAndCategoryCode(predictionIdsRequest);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getPredictionIdAndCategoryCode(any(), Mockito.anyString(), Mockito.anyString(),
              any(), any(), any());
    }
  }

  @Test
  public void upsertPredictionCategoryMappingTest() {
    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    PredictionCategoryMappingRequest predictionCategoryMappingRequest = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest.setCategoryCode(CATEGORY_CODE);
    requestList.add(predictionCategoryMappingRequest);

    Mockito.when(pcbFeign.upsertPredictionCategoryMapping(any(), Mockito.anyString(), Mockito.anyString(),
            any(), any(), any()))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));

    productOutboundBean.upsertPredictionCategoryMapping(requestList);

    Mockito.verify(pcbFeign)
        .upsertPredictionCategoryMapping(any(), Mockito.anyString(), Mockito.anyString(),
            any(), any(), any());
  }

  @Test
  public void upsertPredictionCategoryMappingApplicationRuntimeExceptionTest() throws Exception {
    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    PredictionCategoryMappingRequest predictionCategoryMappingRequest = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest.setCategoryCode(CATEGORY_CODE);
    requestList.add(predictionCategoryMappingRequest);

    Mockito.when(pcbFeign.upsertPredictionCategoryMapping(any(), Mockito.anyString(), Mockito.anyString(),
        any(), any(), any())).thenReturn(
        new GdnBaseRestResponse(ErrorCategory.VALIDATION.getMessage(), ErrorCategory.VALIDATION.getCode(), false,
            REQUEST_ID));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.upsertPredictionCategoryMapping(requestList);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .upsertPredictionCategoryMapping(any(), Mockito.anyString(), Mockito.anyString(),
              any(), any(), any());
    }
  }

  @Test
  public void upsertPredictionCategoryMappingExceptionTest() throws Exception {
    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    PredictionCategoryMappingRequest predictionCategoryMappingRequest = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest.setCategoryCode(CATEGORY_CODE);
    requestList.add(predictionCategoryMappingRequest);

    Mockito.when(pcbFeign.upsertPredictionCategoryMapping(any(), Mockito.anyString(), Mockito.anyString(),
        any(), any(), any())).thenReturn(
        new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
            REQUEST_ID));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.upsertPredictionCategoryMapping(requestList);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .upsertPredictionCategoryMapping(any(), Mockito.anyString(), Mockito.anyString(),
              any(), any(), any());
    }
  }

  @Test
  public void getProductItemImagesByItemCodeTest() {
    ItemImageResponse itemImageResponse = new ItemImageResponse(SKU_CODE, UPC_CODE, ITEM_NAME, new ArrayList<>());
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest(Arrays.asList(SKU_CODE));

    Mockito.when(pcbFeign.getProductItemImagesByItemCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, skuCodesRequest))
        .thenReturn(
            new GdnRestListResponse<>(null, null, true, Arrays.asList(itemImageResponse), new PageMetaData(0, 1, 1),
                REQUEST_ID));

    productOutboundBean.getProductItemImagesByItemCode(skuCodesRequest, false);

    Mockito.verify(pcbFeign).getProductItemImagesByItemCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, skuCodesRequest);
  }

  @Test
  public void getProductItemImagesByItemCodeSuccessFalseTest() {
    ItemImageResponse itemImageResponse = new ItemImageResponse(SKU_CODE, UPC_CODE, ITEM_NAME, new ArrayList<>());
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest(Arrays.asList(SKU_CODE));

    Mockito.when(pcbFeign.getProductItemImagesByItemCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, skuCodesRequest))
        .thenReturn(
            new GdnRestListResponse<>(null, null, false, Arrays.asList(itemImageResponse), new PageMetaData(0, 1, 1),
                REQUEST_ID));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getProductItemImagesByItemCode(skuCodesRequest, false);
      });
    } finally {
      Mockito.verify(pcbFeign).getProductItemImagesByItemCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false,
          skuCodesRequest);
    }
  }

  @Test
  public void getProductAttributesByProductIdTest() {
    Mockito.when(pcbFeign.getProductAttributesByProductId(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new ProductAndAttributeDetailResponse(), REQUEST_ID));
    productOutboundBean.getProductAttributesByProductId(PRODUCT_ID);
    Mockito.verify(pcbFeign).getProductAttributesByProductId(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID);
  }

  @Test
  public void getProductAttributesByProductIdFalseTest() {
    Mockito.when(pcbFeign.getProductAttributesByProductId(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID)).thenReturn(
        new GdnRestSingleResponse<>(null, null, false, new ProductAndAttributeDetailResponse(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getProductAttributesByProductId(PRODUCT_ID);
      });
    } finally {
      Mockito.verify(pcbFeign).getProductAttributesByProductId(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID);
    }
  }

  @Test
  public void getProductAttributesByProductIdNullTest() {
    Mockito.when(pcbFeign.getProductAttributesByProductId(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getProductAttributesByProductId(PRODUCT_ID);
      });
    } finally {
      Mockito.verify(pcbFeign).getProductAttributesByProductId(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID);
    }
  }

  @Test
  public void getProductItemImagesByItemCodeResponseNullTest() {
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest(Arrays.asList(SKU_CODE));

    Mockito.when(pcbFeign.getProductItemImagesByItemCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, skuCodesRequest))
        .thenReturn(
            new GdnRestListResponse<>(null, null, true, null, new PageMetaData(0, 1, 1),
                REQUEST_ID));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getProductItemImagesByItemCode(skuCodesRequest, false);
      });
    } finally {
      Mockito.verify(pcbFeign).getProductItemImagesByItemCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false,
          skuCodesRequest);
    }
  }

  @Test
  public void getProductItemByListOfProductCodeTest() {
    Mockito.when(pcbFeign.getProductItemByListOfProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, false, false,
        null)).thenReturn(
        new GdnRestListResponse<>(null, null, true, List.of(new ProductItemDetailResponse()), new PageMetaData(0, 1, 1),
            REQUEST_ID));
    productOutboundBean.getProductItemByListOfProductCode(null, true, false, false);
    Mockito.verify(pcbFeign).getProductItemByListOfProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, false, false,
        null);
  }

  @Test
  public void getProductItemByListOfProductCodeSuccessFalseTest() {
    Mockito.when(pcbFeign.getProductItemByListOfProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, true, false,
        null)).thenReturn(new GdnRestListResponse<>(null, null, false, List.of(new ProductItemDetailResponse()),
        new PageMetaData(0, 1, 1), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getProductItemByListOfProductCode(null, false, true, false);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getProductItemByListOfProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, true, false,
              null);
    }
  }

  @Test
  public void getProductItemByListOfProductCodeEmptyResponseTest() {
    Mockito.when(pcbFeign.getProductItemByListOfProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, false, true,
        null)).thenReturn(new GdnRestListResponse<>(null, null, true, null, new PageMetaData(0, 1, 1), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getProductItemByListOfProductCode(null, false, false, true);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getProductItemByListOfProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, false, true,
              null);
    }
  }

  @Test
  public void getAttributeDetailByAttributeCodesTest() {
    Mockito.when(pcbFeign.getAttributeDetailByAttributeCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, null)).thenReturn(
        new GdnRestListResponse<>(null, null, true, List.of(new AttributeResponse()), new PageMetaData(0, 1, 1),
            REQUEST_ID));
    productOutboundBean.getAttributeDetailByAttributeCodes(true, null);
    Mockito.verify(pcbFeign)
        .getAttributeDetailByAttributeCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, null);
  }

  @Test
  public void getAttributeDetailByAttributeCodesSuccessFalseTest() {
    Mockito.when(pcbFeign.getAttributeDetailByAttributeCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, null)).thenReturn(
        new GdnRestListResponse<>(null, null, false, List.of(new AttributeResponse()), new PageMetaData(0, 1, 1),
            REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getAttributeDetailByAttributeCodes(true, null);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getAttributeDetailByAttributeCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, null);
    }
  }

  @Test
  public void getAttributeDetailByAttributeCodesEmptyResponseTest() {
    Mockito.when(pcbFeign.getAttributeDetailByAttributeCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, null))
        .thenReturn(new GdnRestListResponse<>(null, null, true, null, new PageMetaData(0, 1, 1), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.getAttributeDetailByAttributeCodes(true, null);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getAttributeDetailByAttributeCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, null);
    }
  }

  @Test
  public void updateProductMasterDataInPCB_SuccessNoImageErrors() {
    ProductMasterDataEditRequest productMasterDataEditRequest = new ProductMasterDataEditRequest();
    productMasterDataEditRequest.setProductCode(PRODUCT_CODE);
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setProductCollection(new ProductCollection());
    ProductMasterDataUpdateResponse updateResponse = new ProductMasterDataUpdateResponse();
    updateResponse.setProductImagesErrorMap(new HashMap<>()); // No image errors

    GdnRestSingleResponse<ProductMasterDataUpdateResponse> pcbResponse = new GdnRestSingleResponse<>();
    pcbResponse.setSuccess(true);
    pcbResponse.setValue(updateResponse);

    Mockito.when(pcbFeign.updateMasterData(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any()))
      .thenReturn(pcbResponse);

    productOutboundBean.updateProductMasterDataInPCB(Constants.DEFAULT_STORE_ID, REQUEST_ID,
      USERNAME, productMasterDataEditRequest, masterProductEditDTO);

    Mockito.verify(pcbFeign).updateMasterData(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any());
  }

  @Test
  public void updateProductMasterDataInPCB_FailureResponseValueNull() {
    ProductMasterDataEditRequest productMasterDataEditRequest = new ProductMasterDataEditRequest();
    productMasterDataEditRequest.setProductCode(PRODUCT_CODE);
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setProductCollection(new ProductCollection());
    GdnRestSingleResponse<ProductMasterDataUpdateResponse> pcbResponse = new GdnRestSingleResponse<>();
    pcbResponse.setSuccess(false);
    pcbResponse.setValue(null);
    pcbResponse.setErrorMessage("Error updating master data");

    Mockito.when(pcbFeign.updateMasterData(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any()))
      .thenReturn(pcbResponse);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateProductMasterDataInPCB(Constants.DEFAULT_STORE_ID, REQUEST_ID,
          USERNAME, productMasterDataEditRequest, masterProductEditDTO);
      });
    } finally {
      Mockito.verify(pcbFeign).updateMasterData(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any());
    }
  }

  @Test
  public void updateProductMasterDataInPCB_FailureWithValidationError() {
    ProductMasterDataEditRequest productMasterDataEditRequest = new ProductMasterDataEditRequest();
    productMasterDataEditRequest.setProductCode(PRODUCT_CODE);
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setProductCollection(new ProductCollection());
    ProductMasterDataUpdateResponse updateResponse = new ProductMasterDataUpdateResponse();
    updateResponse.setErrorMessage(ErrorMessage.SIZE_CHART_IS_NOT_VALID_FOR_THE_CATEGORY_ERROR_CODE);

    GdnRestSingleResponse<ProductMasterDataUpdateResponse> pcbResponse = new GdnRestSingleResponse<>();
    pcbResponse.setSuccess(false);
    pcbResponse.setValue(updateResponse);

    Mockito.when(pcbFeign.updateMasterData(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any()))
      .thenReturn(pcbResponse);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productOutboundBean.updateProductMasterDataInPCB(Constants.DEFAULT_STORE_ID, REQUEST_ID,
          USERNAME, productMasterDataEditRequest, masterProductEditDTO);
      });
    } finally {
      Mockito.verify(pcbFeign).updateMasterData(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any());
    }
  }

  @Test
  public void updateProductMasterDataInPCB_SuccessWithImageErrors() {
    ProductMasterDataEditRequest productMasterDataEditRequest = new ProductMasterDataEditRequest();
    productMasterDataEditRequest.setProductCode(PRODUCT_CODE);
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setProductCollection(new ProductCollection());
    ProductMasterDataUpdateResponse updateResponse = new ProductMasterDataUpdateResponse();
    Map<String, String> imageErrors = new HashMap<>();
    imageErrors.put("image1.jpg", "Invalid format");
    Map<String, Map<String, String>> productImagesErrorMap = new HashMap<>();
    productImagesErrorMap.put(PRODUCT_CODE, imageErrors);
    updateResponse.setProductImagesErrorMap(productImagesErrorMap);

    GdnRestSingleResponse<ProductMasterDataUpdateResponse> pcbResponse = new GdnRestSingleResponse<>();
    pcbResponse.setSuccess(true);
    pcbResponse.setValue(updateResponse);

    Mockito.when(pcbFeign.updateMasterData(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any()))
      .thenReturn(pcbResponse);

    productOutboundBean.updateProductMasterDataInPCB(Constants.DEFAULT_STORE_ID, REQUEST_ID,
      USERNAME, productMasterDataEditRequest, masterProductEditDTO);

    Mockito.verify(pcbFeign).updateMasterData(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), any());
  }

  @Test
  public void getDistributionInfo_Success_SinglePage() {
    // Given
    List<DistributionInfoPerSkuResponse> distributionInfoList = createDistributionInfoList(5);
    
    PageMetaData pageMetaData = new PageMetaData();
    pageMetaData.setTotalRecords(5);
    pageMetaData.setPageSize(100);
    pageMetaData.setPageNumber(0);

    GdnRestListResponse<DistributionInfoPerSkuResponse> response = new GdnRestListResponse<>();
    response.setSuccess(true);
    response.setContent(distributionInfoList);
    response.setPageMetaData(pageMetaData);

    Mockito.when(pcbFeign.getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),  // requestId
        Mockito.anyString(),  // username
        eq(PRODUCT_CODE),
        eq(true),  // firstPage
        eq(0),     // page
        eq(100)    // size
    )).thenReturn(response);

    // When
    List<DistributionInfoPerSkuResponse> result = productOutboundBean.getDistributionInfo(PRODUCT_CODE);

    // Then
    Assertions.assertEquals(5, result.size());
    Assertions.assertEquals("SKU_0", result.get(0).getSkuCode());
    Assertions.assertEquals("SKU_4", result.get(4).getSkuCode());

    Mockito.verify(pcbFeign, Mockito.times(1)).getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(true),
        eq(0),
        eq(100)
    );
  }

  @Test
  public void getDistributionInfo_Success_MultiplePages() {
    // Given - First page response
    List<DistributionInfoPerSkuResponse> firstPageList = createDistributionInfoList(5);

    PageMetaData firstPageMetaData = new PageMetaData();
    firstPageMetaData.setTotalRecords(150); // This will create 2 pages (150/100 = 1.5 -> 2 pages)
    firstPageMetaData.setPageSize(100);
    firstPageMetaData.setPageNumber(0);

    GdnRestListResponse<DistributionInfoPerSkuResponse> firstPageResponse = new GdnRestListResponse<>();
    firstPageResponse.setSuccess(true);
    firstPageResponse.setContent(firstPageList);
    firstPageResponse.setPageMetaData(firstPageMetaData);

    // Given - Second page response
    List<DistributionInfoPerSkuResponse> secondPageList = createDistributionInfoList(3, 5); // Start from index 5

    PageMetaData secondPageMetaData = new PageMetaData();
    secondPageMetaData.setTotalRecords(150);
    secondPageMetaData.setPageSize(100);
    secondPageMetaData.setPageNumber(1);

    GdnRestListResponse<DistributionInfoPerSkuResponse> secondPageResponse = new GdnRestListResponse<>();
    secondPageResponse.setSuccess(true);
    secondPageResponse.setContent(secondPageList);
    secondPageResponse.setPageMetaData(secondPageMetaData);

    // Mock first page call
    Mockito.when(pcbFeign.getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(true),  // firstPage
        eq(0),     // page
        eq(100)    // size
    )).thenReturn(firstPageResponse);

    // Mock second page call
    Mockito.when(pcbFeign.getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(false), // not firstPage
        eq(1),     // page
        eq(100)    // size
    )).thenReturn(secondPageResponse);

    // When
    List<DistributionInfoPerSkuResponse> result = productOutboundBean.getDistributionInfo(PRODUCT_CODE);

    // Then
    Assertions.assertEquals(8, result.size()); // 5 from first page + 3 from second page
    Assertions.assertEquals("SKU_0", result.get(0).getSkuCode());
    Assertions.assertEquals("SKU_7", result.get(7).getSkuCode());

    // Verify both calls were made
    Mockito.verify(pcbFeign).getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(true),
        eq(0),
        eq(100)
    );

    Mockito.verify(pcbFeign).getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(false),
        eq(1),
        eq(100)
    );
  }

  @Test
  public void getDistributionInfo_FirstPageFailure_ThrowsException() {
    // Given
    GdnRestListResponse<DistributionInfoPerSkuResponse> failedResponse = new GdnRestListResponse<>();
    failedResponse.setSuccess(false);
    failedResponse.setErrorMessage("PCB service unavailable");
    failedResponse.setErrorCode("SERVICE_ERROR");

    Mockito.when(pcbFeign.getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(true),
        eq(0),
        eq(100)
    )).thenReturn(failedResponse);

    // When & Then
    ApplicationRuntimeException exception = Assertions.assertThrows(
        ApplicationRuntimeException.class,
        () -> productOutboundBean.getDistributionInfo(PRODUCT_CODE)
    );

    Assertions.assertEquals(ErrorCategory.UNSPECIFIED, exception.getErrorCodes());

    Mockito.verify(pcbFeign).getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(true),
        eq(0),
        eq(100)
    );
  }

  @Test
  public void getDistributionInfo_SecondPageFailure_ThrowsException() {
    // Given - Successful first page
    List<DistributionInfoPerSkuResponse> firstPageList = createDistributionInfoList(5);
    
    PageMetaData firstPageMetaData = new PageMetaData();
    firstPageMetaData.setTotalRecords(150);
    firstPageMetaData.setPageSize(100);
    firstPageMetaData.setPageNumber(0);
    
    GdnRestListResponse<DistributionInfoPerSkuResponse> firstPageResponse = new GdnRestListResponse<>();
    firstPageResponse.setSuccess(true);
    firstPageResponse.setContent(firstPageList);
    firstPageResponse.setPageMetaData(firstPageMetaData);

    // Given - Failed second page
    GdnRestListResponse<DistributionInfoPerSkuResponse> failedSecondPageResponse = new GdnRestListResponse<>();
    failedSecondPageResponse.setSuccess(false);
    failedSecondPageResponse.setErrorMessage("Connection timeout on page 2");

    // Mock first page call (success)
    Mockito.when(pcbFeign.getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(true),
        eq(0),
        eq(100)
    )).thenReturn(firstPageResponse);

    // Mock second page call (failure)
    Mockito.when(pcbFeign.getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(false),
        eq(1),
        eq(100)
    )).thenReturn(failedSecondPageResponse);

    // When & Then
    ApplicationRuntimeException exception = Assertions.assertThrows(
        ApplicationRuntimeException.class,
        () -> productOutboundBean.getDistributionInfo(PRODUCT_CODE)
    );

    Assertions.assertEquals(ErrorCategory.UNSPECIFIED, exception.getErrorCodes());

    // Verify both calls were attempted
    Mockito.verify(pcbFeign).getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(true),
        eq(0),
        eq(100)
    );

    Mockito.verify(pcbFeign).getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(false),
        eq(1),
        eq(100)
    );
  }

  @Test
  public void getDistributionInfo_ExactlyTwoPages_Success() {
    // Given - First page response
    List<DistributionInfoPerSkuResponse> firstPageList = createDistributionInfoList(3);
    
    PageMetaData firstPageMetaData = new PageMetaData();
    firstPageMetaData.setTotalRecords(200); // Exactly 2 pages (200/100 = 2.0)
    firstPageMetaData.setPageSize(100);
    firstPageMetaData.setPageNumber(0);
    
    GdnRestListResponse<DistributionInfoPerSkuResponse> firstPageResponse = new GdnRestListResponse<>();
    firstPageResponse.setSuccess(true);
    firstPageResponse.setContent(firstPageList);
    firstPageResponse.setPageMetaData(firstPageMetaData);

    // Given - Second page response
    List<DistributionInfoPerSkuResponse> secondPageList = createDistributionInfoList(2, 3);
    
    PageMetaData secondPageMetaData = new PageMetaData();
    secondPageMetaData.setTotalRecords(200);
    secondPageMetaData.setPageSize(100);
    secondPageMetaData.setPageNumber(1);
    
    GdnRestListResponse<DistributionInfoPerSkuResponse> secondPageResponse = new GdnRestListResponse<>();
    secondPageResponse.setSuccess(true);
    secondPageResponse.setContent(secondPageList);
    secondPageResponse.setPageMetaData(secondPageMetaData);

    // Mock calls
    Mockito.when(pcbFeign.getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(true),
        eq(0),
        eq(100)
    )).thenReturn(firstPageResponse);

    Mockito.when(pcbFeign.getDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        eq(false),
        eq(1),
        eq(100)
    )).thenReturn(secondPageResponse);

    // When
    List<DistributionInfoPerSkuResponse> result = productOutboundBean.getDistributionInfo(PRODUCT_CODE);

    // Then
    Assertions.assertEquals(5, result.size()); // 3 from first page + 2 from second page
    Assertions.assertEquals("SKU_0", result.get(0).getSkuCode());
    Assertions.assertEquals("SKU_4", result.get(4).getSkuCode());

    // Verify exactly 2 calls were made
    Mockito.verify(pcbFeign, Mockito.times(2)).getDistributionInfo(
        Mockito.anyString(),
        Mockito.anyString(),
        Mockito.anyString(),
        Mockito.anyString(),
        Mockito.anyString(),
        Mockito.eq(PRODUCT_CODE),
        Mockito.anyBoolean(),
        Mockito.anyInt(),
        Mockito.eq(100)
    );
  }

  // ================ Helper Methods for Distribution Info Tests ================

  /**
   * Creates a list of DistributionInfoPerSkuResponse for testing
   */
  private List<DistributionInfoPerSkuResponse> createDistributionInfoList(int count) {
    return createDistributionInfoList(count, 0);
  }

  /**
   * Creates a list of DistributionInfoPerSkuResponse for testing with starting index
   */
  private List<DistributionInfoPerSkuResponse> createDistributionInfoList(int count, int startIndex) {
    List<DistributionInfoPerSkuResponse> list = new ArrayList<>();
    
    for (int i = 0; i < count; i++) {
      DistributionInfoPerSkuResponse distributionInfo = new DistributionInfoPerSkuResponse();
      distributionInfo.setSkuCode("SKU_" + (startIndex + i));
      
      // Create DistributionInfoResponse
      DistributionInfoResponse distributionInfoResponse = new DistributionInfoResponse();
      distributionInfoResponse.setProductName("Product " + (startIndex + i));
      distributionInfoResponse.setCategoryName("Category " + (startIndex + i));
      distributionInfo.setDistributionInfoResponse(distributionInfoResponse);
      
      // Create DistributionItemInfoResponse
      DistributionItemInfoResponse distributionItemInfoResponse = new DistributionItemInfoResponse();
      distributionItemInfoResponse.setOmniChannelSku("OMNI_" + (startIndex + i));
      distributionItemInfoResponse.setOrigin("LOCAL");
      distributionItemInfoResponse.setExpiry(false);
      distributionInfo.setDistributionItemInfoResponse(distributionItemInfoResponse);
      
      // Create DimensionsAndUomResponse list
      List<DimensionsAndUomResponse> dimensionsAndUomResponses = new ArrayList<>();
      DimensionsAndUomResponse dimensionsAndUomResponse = new DimensionsAndUomResponse();
      dimensionsAndUomResponse.setUomCode("PCS");
      dimensionsAndUomResponse.setConversion(1.0);
      dimensionsAndUomResponse.setLength(10.0 + i);
      dimensionsAndUomResponse.setWidth(5.0 + i);
      dimensionsAndUomResponse.setHeight(2.0 + i);
      dimensionsAndUomResponse.setUpcEanList(Arrays.asList("UPC" + (startIndex + i)));
      dimensionsAndUomResponses.add(dimensionsAndUomResponse);
      distributionInfo.setDimensionsAndUomResponse(dimensionsAndUomResponses);
      
      list.add(distributionInfo);
    }
    
    return list;
  }

  // ===============================
  // Tests for getOmniChannelSkuToItemCode method
  // ===============================

  @Test
  public void getOmniChannelSkuToItemCode_Success_NonNullCase() {
    // Given
    List<String> omniChannelSkus = Arrays.asList(OMNI_CHANNEL_SKU_1, OMNI_CHANNEL_SKU_2);
    Map<String, ProductL1AndL2CodeResponse> expectedMap = createOmniChannelSkuMap();
    
    ValidOmniChannelSkuResponse validResponse = new ValidOmniChannelSkuResponse();
    validResponse.setExistingOmniChannelSkusAndProductDetailsMap(expectedMap);
    
    GdnRestSingleResponse<ValidOmniChannelSkuResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(validResponse);
    
    Mockito.when(pcbFeign.checkOmniChannelSkuExistsInSeller(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.anyString(),
        Mockito.anyString(),
        any(OmniChannelSkuRequest.class)
    )).thenReturn(response);
    
    // When
    Map<String, ProductL1AndL2CodeResponse> result = 
        productOutboundBean.getOmniChannelSkuToItemCode(SELLER_CODE, omniChannelSkus);
    
    // Then
    Assertions.assertNotNull(result);
    Assertions.assertEquals(expectedMap, result);
    
    Mockito.verify(pcbFeign).checkOmniChannelSkuExistsInSeller(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.anyString(),
        Mockito.anyString(),
        any(OmniChannelSkuRequest.class)
    );
  }

  @Test
  public void getOmniChannelSkuToItemCode_Failure_SuccessFalse() {
    // Given
    List<String> omniChannelSkus = Arrays.asList(OMNI_CHANNEL_SKU_1);
    
    GdnRestSingleResponse<ValidOmniChannelSkuResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    response.setErrorMessage("PCB service unavailable");
    
    Mockito.when(pcbFeign.checkOmniChannelSkuExistsInSeller(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.anyString(),
        Mockito.anyString(),
        any(OmniChannelSkuRequest.class)
    )).thenReturn(response);
    
    // When & Then
    ApplicationRuntimeException exception = Assertions.assertThrows(
        ApplicationRuntimeException.class,
        () -> productOutboundBean.getOmniChannelSkuToItemCode(SELLER_CODE, omniChannelSkus)
    );
    
    Assertions.assertEquals(ErrorCategory.UNSPECIFIED, exception.getErrorCodes());
    Assertions.assertEquals("Unspecified error :PCB service unavailable", exception.getErrorMessage());
    
    Mockito.verify(pcbFeign).checkOmniChannelSkuExistsInSeller(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.anyString(),
        Mockito.anyString(),
        any(OmniChannelSkuRequest.class)
    );
  }

  @Test
  public void getOmniChannelSkuToItemCode_Failure_ResponseValueNull() {
    // Given
    List<String> omniChannelSkus = Arrays.asList(OMNI_CHANNEL_SKU_1);
    
    GdnRestSingleResponse<ValidOmniChannelSkuResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(null);
    response.setErrorMessage("Unspecified error :No data found");
    
    Mockito.when(pcbFeign.checkOmniChannelSkuExistsInSeller(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.anyString(),
        Mockito.anyString(),
        any(OmniChannelSkuRequest.class)
    )).thenReturn(response);
    
    // When & Then
    ApplicationRuntimeException exception = Assertions.assertThrows(
        ApplicationRuntimeException.class,
        () -> productOutboundBean.getOmniChannelSkuToItemCode(SELLER_CODE, omniChannelSkus)
    );
    
    Assertions.assertEquals(ErrorCategory.UNSPECIFIED, exception.getErrorCodes());

    
    Mockito.verify(pcbFeign).checkOmniChannelSkuExistsInSeller(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.anyString(),
        Mockito.anyString(),
        any(OmniChannelSkuRequest.class)
    );
  }

  // ===============================
  // Tests for updateDistributionInfo method
  // ===============================

  @Test
  public void updateDistributionInfo_Success_NonNullCase() {
    // Given
    DistributionInfoUpdateRequest distributionInfoUpdateRequest = createDistributionInfoUpdateRequest();
    
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    
    Mockito.when(pcbFeign.updateDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        any(DistributionInfoUpdateRequest.class)
    )).thenReturn(response);
    
    // When & Then - No exception should be thrown
    Assertions.assertDoesNotThrow(() -> 
        productOutboundBean.updateDistributionInfo(PRODUCT_CODE, distributionInfoUpdateRequest)
    );
    
    Mockito.verify(pcbFeign).updateDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        any(DistributionInfoUpdateRequest.class)
    );
  }

  @Test
  public void updateDistributionInfo_Failure_SuccessFalse() {
    // Given
    DistributionInfoUpdateRequest distributionInfoUpdateRequest = createDistributionInfoUpdateRequest();
    
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(false);
    response.setErrorMessage("Distribution info update failed");
    
    Mockito.when(pcbFeign.updateDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        any(DistributionInfoUpdateRequest.class)
    )).thenReturn(response);
    
    // When & Then
    ApplicationRuntimeException exception = Assertions.assertThrows(
        ApplicationRuntimeException.class,
        () -> productOutboundBean.updateDistributionInfo(PRODUCT_CODE, distributionInfoUpdateRequest)
    );
    
    Assertions.assertEquals(ErrorCategory.UNSPECIFIED, exception.getErrorCodes());
    Assertions.assertEquals("Unspecified error :Distribution info update failed", exception.getErrorMessage());
    
    Mockito.verify(pcbFeign).updateDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        any(DistributionInfoUpdateRequest.class)
    );
  }

  @Test
  public void updateDistributionInfo_Failure_ResponseNull() {
    // Given
    DistributionInfoUpdateRequest distributionInfoUpdateRequest = createDistributionInfoUpdateRequest();
    
    Mockito.when(pcbFeign.updateDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        any(DistributionInfoUpdateRequest.class)
    )).thenReturn(null);

    Assertions.assertThrows(NullPointerException.class,
        () -> productOutboundBean.updateDistributionInfo(PRODUCT_CODE, distributionInfoUpdateRequest));
    
    Mockito.verify(pcbFeign).updateDistributionInfo(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(PRODUCT_CODE),
        any(DistributionInfoUpdateRequest.class)
    );
  }

  // ===============================
  // Helper methods for test data creation
  // ===============================

  private Map<String, ProductL1AndL2CodeResponse> createOmniChannelSkuMap() {
    Map<String, ProductL1AndL2CodeResponse> map = new HashMap<>();
    map.put(OMNI_CHANNEL_SKU_1, createProductL1AndL2CodeResponse(ITEM_CODE_1));
    map.put(OMNI_CHANNEL_SKU_2, createProductL1AndL2CodeResponse(ITEM_CODE_2));
    return map;
  }

  private ProductL1AndL2CodeResponse createProductL1AndL2CodeResponse(String itemCode) {
    ProductL1AndL2CodeResponse response = new ProductL1AndL2CodeResponse();
    response.setSkuCode(itemCode);
    response.setProductCode(PRODUCT_CODE);
    return response;
  }

  private DistributionInfoUpdateRequest createDistributionInfoUpdateRequest() {
    DistributionInfoUpdateRequest request = new DistributionInfoUpdateRequest();
    // Set required fields for the request object
    // Note: Add specific fields based on the DistributionInfoUpdateRequest structure
    return request;
  }

  @Test
  public void updateProductBrandValueTest_success() {
    ProductBrandUpdateRequest productBrandUpdateRequest =
        ProductBrandUpdateRequest.builder().newBrandCode(BRAND_CODE).productCode(PRODUCT_CODE)
            .newBrandName(BRAND_CODE).build();
    Mockito.when(
            pcbFeign.updateProductBrandName(Constants.DEFAULT_STORE_ID,
                Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
                Constants.DEFAULT_USERNAME,
                productBrandUpdateRequest))
        .thenReturn(new GdnRestSingleResponse<>(new ProductBrandUpdateResponse(), REQUEST_ID));
    productOutboundBean.updateProductBrandValue(productBrandUpdateRequest);
    Mockito.verify(pcbFeign).updateProductBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        productBrandUpdateRequest);
  }

  @Test
  public void updateProductBrandValueTest_validationException() {
    ProductBrandUpdateRequest productBrandUpdateRequest =
        ProductBrandUpdateRequest.builder().newBrandCode(BRAND_CODE).productCode(PRODUCT_CODE)
            .newBrandName(BRAND_CODE).build();
    Mockito.when(
        pcbFeign.updateProductBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productBrandUpdateRequest)).thenReturn(
        new GdnRestSingleResponse<>("Seller is not authorised", "ERR-123", false, null, "123"));
    try {
      productOutboundBean.updateProductBrandValue(productBrandUpdateRequest);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals("Can not process invalid input data :Seller is not authorised",
          e.getErrorMessage());
    }
    Mockito.verify(pcbFeign)
        .updateProductBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productBrandUpdateRequest);
  }

  @Test
  public void updateProductBrandValueTest_emptyErrorMessage() {
    ProductBrandUpdateRequest productBrandUpdateRequest =
        ProductBrandUpdateRequest.builder().newBrandCode(BRAND_CODE).productCode(PRODUCT_CODE)
            .newBrandName(BRAND_CODE).build();
    Mockito.when(
        pcbFeign.updateProductBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productBrandUpdateRequest)).thenReturn(
        new GdnRestSingleResponse<>(null, null, false, null, "123"));
    try {
      productOutboundBean.updateProductBrandValue(productBrandUpdateRequest);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals("Unspecified error :", e.getErrorMessage());
    }
    Mockito.verify(pcbFeign)
        .updateProductBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productBrandUpdateRequest);
  }

  @Test
  public void updateProductBrandValueTest_nullResponse() {
    ProductBrandUpdateRequest productBrandUpdateRequest =
        ProductBrandUpdateRequest.builder().newBrandCode(BRAND_CODE).productCode(PRODUCT_CODE)
            .newBrandName(BRAND_CODE).build();
    Mockito.when(
        pcbFeign.updateProductBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productBrandUpdateRequest))
        .thenReturn(null);
    try {
      productOutboundBean.updateProductBrandValue(productBrandUpdateRequest);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals("Unspecified error :", e.getErrorMessage());
    }
    Mockito.verify(pcbFeign)
        .updateProductBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productBrandUpdateRequest);
  }

  @Test
  public void updateProductBrandValueTest_nullValue() {
    ProductBrandUpdateRequest productBrandUpdateRequest =
        ProductBrandUpdateRequest.builder().newBrandCode(BRAND_CODE).productCode(PRODUCT_CODE)
            .newBrandName(BRAND_CODE).build();
    Mockito.when(
            pcbFeign.updateProductBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
                productBrandUpdateRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      productOutboundBean.updateProductBrandValue(productBrandUpdateRequest);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals("Unspecified error :", e.getErrorMessage());
    }
    Mockito.verify(pcbFeign)
        .updateProductBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            productBrandUpdateRequest);
  }
}
