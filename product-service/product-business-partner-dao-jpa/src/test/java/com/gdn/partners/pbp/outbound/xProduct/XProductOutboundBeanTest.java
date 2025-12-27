package com.gdn.partners.pbp.outbound.xProduct;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.MockitoAnnotations.initMocks;

import org.mockito.ArgumentCaptor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.gda.mta.product.dto.CogsUpdateRequests;
import com.gda.mta.product.dto.EditFlagChangesDTO;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.NeedCorrectionProductActivationRequest;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gdn.partners.pbp.outbound.xProduct.feign.ProductBasicMasterFieldsRequest;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.vo.BundleRecipeRequest;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRequest;
import com.gdn.x.product.rest.web.model.request.CogsUpdateListRequest;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointDeleteRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointL5Response;
import com.gdn.x.product.rest.web.model.response.MinMaxItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;

import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.CogsResponse;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.xProduct.feign.XProductFeign;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.SimpleStringBooleanMapRequest;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemsSummaryDetailRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerResponse;
import com.gdn.x.product.rest.web.model.response.DeleteOfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductTypeResponse;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import com.gdn.x.product.rest.web.model.util.GdnRestSimpleResponse;

public class XProductOutboundBeanTest {

  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-00001";
  private static final String DEFAULT_ITEM_CODE = "MTA-00001-00001";
  private static final String DEFAULT_MERCHANT_CODE = "BLI-00001";
  private static final String DEFAULT_MERCHANT_SKU = "merchantSku";
  private static final String DEFAULT_ITEM_SKU = "itemSku";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final boolean includeMarkForDelete = false;
  private static final String TYPE = "type";
  private static final Long COUNT = 1000000000L;
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final PageRequest DEFAULT_PAGE_REQUEST = PageRequest.of(PAGE ,SIZE);
  private static final SortOrder DEFAULT_SORT = new SortOrder("createdDate", "desc");
  private static final String REQUEST_ID = "req-id";
  private static final Set<String> productSkuList = new HashSet<>();
  private static final String ITEM_SKU_2 = "itemSku2";

  private ProductRequest productRequest = new ProductRequest();
  private ProductResponse productResponse = new ProductResponse();
  private ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest;

  private ItemRequest itemRequest = new ItemRequest();
  private ItemResponse itemResponse = new ItemResponse();

  private List<DeleteOfflineItemRequest> deleteOfflineItemRequests;
  private List<DeleteOfflineItemResponse> deleteOfflineItemResponses;
  private List<BusinessPartnerResponse> businessPartnerResponses;
  private List<String> businessPartnerCodes = new ArrayList<>();
  private SimpleListStringRequest simpleListStringRequest;
  private ProductCountResponse productCountResponse;
  private ProductEditRequest productEditRequest;
  private BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse =
    new BusinessPartnerPickupPointResponse();
  private ItemPickupPointSummaryRequest itemPickupPointSummaryRequest =
    new ItemPickupPointSummaryRequest();
  private ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
  private ItemRequestV2 itemRequestV2 = new ItemRequestV2();
  private DeleteItemPickupPointRequest deleteItemPickupPointRequest =
    new DeleteItemPickupPointRequest();
  private List<DeleteItemPickupPointResponse> deletePPCodeResponse;
  private EditProductResponse editProductResponse;
  private ProductCenterDetailResponse productCenterDetailResponse;

  @Mock
  private XProductFeign xProductFeign;

  @InjectMocks
  private xProductOutboundBean xProductOutboundBean;

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
    productRequest.setProductCode(DEFAULT_PRODUCT_CODE);
    productRequest.setMerchantCode(DEFAULT_MERCHANT_CODE);
    productResponse.setMerchantCode(DEFAULT_MERCHANT_CODE);
    productResponse.setProductCode(DEFAULT_PRODUCT_CODE);
    productSkuList.add(PRODUCT_SKU);

    itemRequest.setItemCode(DEFAULT_ITEM_CODE);
    itemResponse.setMerchantCode(DEFAULT_MERCHANT_CODE);
    itemResponse.setItemCode(DEFAULT_ITEM_CODE);

    itemViewConfigAndItemSkuRequest = new ItemViewConfigAndItemSkuRequest();
    itemViewConfigAndItemSkuRequest.setItemSku(DEFAULT_ITEM_SKU);
    itemViewConfigAndItemSkuRequest.setBuyable(true);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);

    deleteOfflineItemRequests = Collections.singletonList(new DeleteOfflineItemRequest());
    deleteOfflineItemResponses = Collections.singletonList(new DeleteOfflineItemResponse());

    businessPartnerCodes.add(DEFAULT_MERCHANT_CODE);

    simpleListStringRequest = new SimpleListStringRequest(businessPartnerCodes);
    businessPartnerResponses = Collections.singletonList(new BusinessPartnerResponse());
    productCountResponse = new ProductCountResponse();
    productCountResponse.setOutOfStock(COUNT);

    productEditRequest = new ProductEditRequest();
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(DEFAULT_ITEM_SKU);


    deletePPCodeResponse = Collections.singletonList(new DeleteItemPickupPointResponse());
    editProductResponse = new EditProductResponse();
    ProductDetailEditDTO productDetailEditDTO = new ProductDetailEditDTO();
    productDetailEditDTO.setProductDetailEditRequestForXProduct(new ProductDetailPageEditRequest());
    editProductResponse.setProductDetailEditDTO(productDetailEditDTO);
    productCenterDetailResponse=new ProductCenterDetailResponse();
    productCenterDetailResponse.setProductName(PRODUCT_CODE);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(xProductFeign);
  }

  @Test
  public void updateProductTest() throws Exception {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    productRequest.setPreOrder(preOrderDTO);
    ArgumentCaptor<ProductRequest> captor = ArgumentCaptor.forClass(ProductRequest.class);
    Mockito.when(this.xProductFeign
        .updateProduct(eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), any(ProductRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productResponse, DEFAULT_REQUEST_ID));
    ProductResponse response =
        xProductOutboundBean.updateProduct(false, productRequest);
    Mockito.verify(this.xProductFeign)
        .updateProduct(eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), captor.capture());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(DEFAULT_MERCHANT_CODE, response.getMerchantCode());
    Assertions.assertNotNull(captor.getValue().getPreOrder());
    Assertions.assertTrue((Boolean) ReflectionTestUtils.getField(captor.getValue().getPreOrder(), "convertToJKT"));
  }

  @Test
  public void updateProduct_exception() throws Exception {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    productRequest.setPreOrder(preOrderDTO);
    ArgumentCaptor<ProductRequest> captor = ArgumentCaptor.forClass(ProductRequest.class);
    Mockito.when(this.xProductFeign
        .updateProduct(eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), any(ProductRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateProduct(false, productRequest);
      });
    } catch (Exception e) {
      throw(e);
    } finally {
      Mockito.verify(this.xProductFeign)
          .updateProduct(eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
              eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
              eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), captor.capture());
      Assertions.assertNotNull(captor.getValue().getPreOrder());
      Assertions.assertTrue((Boolean) ReflectionTestUtils.getField(captor.getValue().getPreOrder(), "convertToJKT"));
    }
  }

  @Test
  public void updateItemTest() throws Exception {
    Mockito.when(this.xProductFeign
        .updateItem(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, false, false, itemRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, itemResponse, DEFAULT_REQUEST_ID));
    ItemResponse response =
        xProductOutboundBean.updateItem(false, false, false, itemRequest);
    Mockito.verify(this.xProductFeign)
        .updateItem(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, false, false,itemRequest);
    Assertions.assertEquals(DEFAULT_ITEM_CODE, response.getItemCode());
    Assertions.assertEquals(DEFAULT_MERCHANT_CODE, response.getMerchantCode());
  }

  @Test
  public void updateItem_exception() throws Exception {
    Mockito.when(this.xProductFeign
        .updateItem(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, true, false, itemRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateItem(false, true, false, itemRequest);
      });
    } catch (Exception e) {
      throw(e);
    } finally {
      Mockito.verify(this.xProductFeign)
          .updateItem(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), false, true, false, itemRequest);
    }
  }

  @Test
  public void updateItemViewConfigAndForceReviewTest() {
    Mockito.when(xProductFeign.updateItemViewConfigAndForceReview(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), false, false, false,
        Collections.singletonList(itemViewConfigAndItemSkuRequest))).thenReturn(new GdnBaseRestResponse(true));
    xProductOutboundBean.updateItemViewConfigAndForceReview(false,
        Collections.singletonList(itemViewConfigAndItemSkuRequest), false);
    Mockito.verify(xProductFeign)
        .updateItemViewConfigAndForceReview(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, false, false,
            Collections.singletonList(itemViewConfigAndItemSkuRequest));
  }

  @Test
  public void updateItemViewConfigAndForceReviewExceptionTest() {
    Mockito.when(xProductFeign.updateItemViewConfigAndForceReview(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), false, false,  false,
        Collections.singletonList(itemViewConfigAndItemSkuRequest))).thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateItemViewConfigAndForceReview(false,
            Collections.singletonList(itemViewConfigAndItemSkuRequest), false);
      });
    } finally {
      Mockito.verify(xProductFeign).updateItemViewConfigAndForceReview(GdnMandatoryRequestParameterUtil.getStoreId(),
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), false, false,  false,
          Collections.singletonList(itemViewConfigAndItemSkuRequest));
    }
  }

  @Test
  public void updateItemViewConfigAndForceReviewWithScheduleRemovalTest() {
    Mockito.when(xProductFeign.updateItemViewConfigAndForceReview(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), false, false, true,
        Collections.singletonList(itemViewConfigAndItemSkuRequest))).thenReturn(new GdnBaseRestResponse(true));
    xProductOutboundBean.updateItemViewConfigAndForceReview(false,
        Collections.singletonList(itemViewConfigAndItemSkuRequest), false, true);
    Mockito.verify(xProductFeign)
        .updateItemViewConfigAndForceReview(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, false, true,
            Collections.singletonList(itemViewConfigAndItemSkuRequest));
  }

  @Test
  public void updateItemViewConfigAndForceReviewWithScheduleRemovalExceptionTest() {
    Mockito.when(xProductFeign.updateItemViewConfigAndForceReview(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), false, false,  false,
        Collections.singletonList(itemViewConfigAndItemSkuRequest))).thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateItemViewConfigAndForceReview(false,
            Collections.singletonList(itemViewConfigAndItemSkuRequest), false, false);
      });
    } finally {
      Mockito.verify(xProductFeign).updateItemViewConfigAndForceReview(GdnMandatoryRequestParameterUtil.getStoreId(),
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), false, false,  false,
          Collections.singletonList(itemViewConfigAndItemSkuRequest));
    }
  }

  @Test
  public void getProductAndItemsTest() {
    Mockito.when(xProductFeign
        .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new ProductAndItemsResponse(),
            GdnMandatoryRequestParameterUtil.getRequestId()));
    GdnRestSingleResponse response = xProductOutboundBean.getProductAndItems(false, PRODUCT_SKU, true);
    Mockito.verify(xProductFeign)
        .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true);
  }

  @Test
  public void getProductAndItemsTestExceptionTest() {
    Mockito.when(xProductFeign
        .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getProductAndItems(false, PRODUCT_SKU, true);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true);
    }
  }

  @Test
  public void bulkDeleteOfflineItemTest() throws Exception {
    GdnRestListResponse<DeleteOfflineItemResponse> response = new GdnRestListResponse<>();
    response.setContent(deleteOfflineItemResponses);
    response.setSuccess(Boolean.TRUE);

    Mockito.when(xProductFeign.bulkDeleteOfflineItem(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE, deleteOfflineItemRequests))
        .thenReturn(response);
    xProductOutboundBean.bulkDeleteOfflineItem(DEFAULT_MERCHANT_CODE, deleteOfflineItemRequests);
    Mockito.verify(xProductFeign).bulkDeleteOfflineItem(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE, deleteOfflineItemRequests);
  }

  @Test
  public void bulkDeleteOfflineItemWithExceptionTest() throws Exception {
    Mockito.when(xProductFeign.bulkDeleteOfflineItem(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE, deleteOfflineItemRequests))
        .thenReturn(new GdnRestListResponse<>());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.bulkDeleteOfflineItem(DEFAULT_MERCHANT_CODE, deleteOfflineItemRequests);
      });
    } finally {
      Mockito.verify(xProductFeign).bulkDeleteOfflineItem(GdnMandatoryRequestParameterUtil.getStoreId(),
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE, deleteOfflineItemRequests);
    }
  }

  @Test
  public void updateMigratedProductCodeTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    Mockito.when(xProductFeign.updateMigratedProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU, PRODUCT_SKU, true))
        .thenReturn(response);
    xProductOutboundBean.updateMigratedProductCode(PRODUCT_SKU, PRODUCT_SKU, true);
    Mockito.verify(xProductFeign).updateMigratedProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU, PRODUCT_SKU, true);
  }

  @Test
  public void updateMigratedProductCodeExceptionTest() throws Exception {
    Mockito.when(xProductFeign
        .updateMigratedProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU, PRODUCT_SKU, true))
        .thenReturn(new GdnBaseRestResponse());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateMigratedProductCode(PRODUCT_SKU, PRODUCT_SKU, true);
      });

    } finally {
      Mockito.verify(xProductFeign)
          .updateMigratedProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU, PRODUCT_SKU, true);
    }
  }

  @Test
  public void getBusinessPartnerDetailsTest() throws Exception {
    GdnRestListResponse<BusinessPartnerResponse> response = new GdnRestListResponse<>();
    response.setContent(businessPartnerResponses);
    response.setSuccess(Boolean.TRUE);

    Mockito.when(xProductFeign.getBusinessPartnerDetails(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, simpleListStringRequest))
        .thenReturn(response);
    List<BusinessPartnerResponse> businessPartnerResponses =
        xProductOutboundBean.getBusinessPartnerDetails(businessPartnerCodes);
    Mockito.verify(xProductFeign).getBusinessPartnerDetails(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, simpleListStringRequest);
    Assertions.assertEquals(1, businessPartnerResponses.size());
  }

  @Test
  public void getBusinessPartnerDetailsExceptionTest() throws Exception {
    Mockito.when(xProductFeign.getBusinessPartnerDetails(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, simpleListStringRequest))
        .thenReturn(new GdnRestListResponse<>());
    List<BusinessPartnerResponse> businessPartnerResponses =
        xProductOutboundBean.getBusinessPartnerDetails(businessPartnerCodes);
    Mockito.verify(xProductFeign).getBusinessPartnerDetails(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, simpleListStringRequest);
    Assertions.assertEquals(0, businessPartnerResponses.size());
  }

  @Test
  public void getProductAndItemsWithProductDataTest() {
    Mockito.when(
        xProductFeign.getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true, true, false)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new ProductAndItemsResponse(),
            GdnMandatoryRequestParameterUtil.getRequestId()));
    GdnRestSingleResponse response = xProductOutboundBean.getProductAndItemsWithProductData(false, PRODUCT_SKU, true,
        false);
    Mockito.verify(xProductFeign)
        .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true, true, false);
  }

  @Test
  public void getProductAndItemsWithProductDataExceptionTest() {
    Mockito.when(
            xProductFeign.getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true, true, false))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getProductAndItemsWithProductData(false, PRODUCT_SKU, true, false);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true, true, false);
    }
  }

  @Test
  public void getProductSkuSummaryTest() {
    Mockito.when(xProductFeign
        .getProductSkuSummary(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            DEFAULT_MERCHANT_CODE, 0, 10, new ProductSkuSummaryRequest())).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(),
            GdnMandatoryRequestParameterUtil.getRequestId()));
    Page<ProductSkuSummaryResponse> response = xProductOutboundBean
        .getProductSkuSummary(DEFAULT_MERCHANT_CODE, new ProductSkuSummaryRequest(), 0, 10);
    Mockito.verify(xProductFeign)
        .getProductSkuSummary(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            DEFAULT_MERCHANT_CODE, 0, 10, new ProductSkuSummaryRequest());
  }

  @Test
  public void getProductSkuSummaryExceptionTest() {
    Mockito.when(xProductFeign
        .getProductSkuSummary(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            DEFAULT_MERCHANT_CODE, 0, 10, new ProductSkuSummaryRequest())).thenReturn(
        new GdnRestListResponse<>(null, null, false, null, null,
            GdnMandatoryRequestParameterUtil.getRequestId()));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean
            .getProductSkuSummary(DEFAULT_MERCHANT_CODE, new ProductSkuSummaryRequest(), 0, 10);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getProductSkuSummary(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
              DEFAULT_MERCHANT_CODE, 0, 10, new ProductSkuSummaryRequest());
    }
  }

  @Test
  public void updatePickupPointCodesTest() throws Exception {
    PickupPointUpdateRequest request = new PickupPointUpdateRequest();
    GdnRestSingleResponse<SimpleLongResponse> response =
        new GdnRestSingleResponse<>(null, null, true, new SimpleLongResponse(Long.valueOf(1L)), DEFAULT_REQUEST_ID);
    response.setSuccess(true);
    Mockito.when(xProductFeign
        .updatePickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), request)).thenReturn(response);
    xProductOutboundBean.updatePickupPointCodes(request);
    Mockito.verify(xProductFeign)
        .updatePickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), request);
  }

  @Test
  public void updatePickupPointCodesExceptionTest() throws Exception {
    PickupPointUpdateRequest request = new PickupPointUpdateRequest();
    GdnRestSingleResponse<SimpleLongResponse> response =
        new GdnRestSingleResponse<>(null, null, false, null, DEFAULT_REQUEST_ID);
    Mockito.when(xProductFeign
        .updatePickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), request)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updatePickupPointCodes(request);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .updatePickupPointCodes(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), request);
    }
  }

  @Test
  public void getPickupPointCodesByProductSkuTest() throws Exception {
    Mockito.when(xProductFeign
        .getPickupPointCodesByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(new ProductPickupPointListResponse(), DEFAULT_REQUEST_ID));
    xProductOutboundBean.getPickupPointCodesByProductSku(PRODUCT_SKU);
    Mockito.verify(xProductFeign)
        .getPickupPointCodesByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
  }

  @Test
  public void getPickupPointCodesByProductSkuExceptionTest() throws Exception {
    Mockito.when(xProductFeign
        .getPickupPointCodesByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU)).thenReturn(
        new GdnRestSingleResponse<>(null, null, false, new ProductPickupPointListResponse(), DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getPickupPointCodesByProductSku(PRODUCT_SKU);
      });

    } finally {
      Mockito.verify(xProductFeign)
          .getPickupPointCodesByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
    }
  }

  @Test
  public void findSummaryDetailsByFilterTest() throws Exception {
    GdnRestListResponse<ItemSummaryDetailResponse> response =
        new GdnRestListResponse<ItemSummaryDetailResponse>(new ArrayList<>(), new PageMetaData(0, 0, 0),
            DEFAULT_REQUEST_ID);
    Mockito.when(xProductFeign
        .getItemsSummaryDetailByFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
            Mockito.anyString(), Mockito.any())).thenReturn(response);
    xProductOutboundBean.findSummaryDetailsByFilter(new ItemsSummaryDetailRequest(), DEFAULT_PAGE_REQUEST);
    Mockito.verify(xProductFeign)
        .getItemsSummaryDetailByFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void findSummaryDetailsByFilterTestException() throws Exception {
    GdnRestListResponse<ItemSummaryDetailResponse> response =
        new GdnRestListResponse<ItemSummaryDetailResponse>("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            DEFAULT_REQUEST_ID);
    Mockito.when(xProductFeign
        .getItemsSummaryDetailByFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
            Mockito.anyString(), Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        xProductOutboundBean.findSummaryDetailsByFilter(new ItemsSummaryDetailRequest(), DEFAULT_PAGE_REQUEST);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getItemsSummaryDetailByFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
              Mockito.anyString(), Mockito.any());
    }
  }

  @Test
  public void getProductDetailsByProductSkuTest() throws Exception {
    Mockito.when(xProductFeign
        .getProductDetailsByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new ProductL3Response(),
            GdnMandatoryRequestParameterUtil.getRequestId()));
    GdnRestSingleResponse response = xProductOutboundBean.getProductDetailsByProductSku(PRODUCT_SKU);
    Mockito.verify(xProductFeign)
        .getProductDetailsByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
  }

  @Test
  public void getProductDetailsByProductSkuExceptionTest() throws Exception {
    Mockito.when(xProductFeign
        .getProductDetailsByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getProductDetailsByProductSku(PRODUCT_SKU);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getProductDetailsByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
    }
  }

  @Test
  public void toggleArchiveProductTest() throws Exception {
    Mockito.when(xProductFeign.toggleArchiveProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), true,
        PRODUCT_SKU)).thenReturn(new GdnBaseRestResponse(true));
    xProductOutboundBean.toggleArchiveProduct(PRODUCT_SKU, true);
    Mockito.verify(xProductFeign).toggleArchiveProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), true,
        PRODUCT_SKU);
  }

  @Test
  public void toggleArchiveProductExceptionTest() throws Exception {
    Mockito.when(xProductFeign.toggleArchiveProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), true,
        PRODUCT_SKU)).thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.toggleArchiveProduct(PRODUCT_SKU, true);
      });
    } finally {
      Mockito.verify(xProductFeign).toggleArchiveProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), true,
          PRODUCT_SKU);
    }
  }

  @Test
  public void updateItemListingTest() throws Exception {
    Mockito.when(xProductFeign
        .updateItemListing(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU,
            new ItemListingUpdateRequest(ProductType.REGULAR, new ArrayList<>())))
        .thenReturn(new GdnBaseRestResponse(null, null, true, DEFAULT_REQUEST_ID));
    xProductOutboundBean.updateItemListing(PRODUCT_SKU, ProductType.REGULAR, new ArrayList<>());
    Mockito.verify(xProductFeign)
        .updateItemListing(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU,
            new ItemListingUpdateRequest(ProductType.REGULAR, new ArrayList<>()));
  }

  @Test
  public void getL5ItemListingTest() throws Exception {
    Mockito.when(xProductFeign.getL5ItemList(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), true, false, null,
            new ItemLevel4ListingWebRequest(productSkuList, Collections.singletonList(PRODUCT_SKU),
                Collections.singletonList(PRODUCT_SKU))))
        .thenReturn(new GdnRestListResponse<ItemLevel5Response>(null, null, true, REQUEST_ID));
    xProductOutboundBean.getL5ItemListing(productSkuList, Collections.singletonList(PRODUCT_SKU),
        Collections.singletonList(PRODUCT_SKU), false, null);
    Mockito.verify(xProductFeign)
        .getL5ItemList(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(), true,
            false, null, new ItemLevel4ListingWebRequest(productSkuList, Collections.singletonList(PRODUCT_SKU),
                Collections.singletonList(PRODUCT_SKU)));
  }

  @Test
  public void getL5ItemListingTest_WhenException() throws Exception {
    Mockito.when(xProductFeign.getL5ItemList(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), true, false, null,
            new ItemLevel4ListingWebRequest(productSkuList, Collections.singletonList(PRODUCT_SKU),
                Collections.singletonList(PRODUCT_SKU))))
        .thenReturn(new GdnRestListResponse<ItemLevel5Response>(null, null, false, REQUEST_ID));
    boolean isSuccess = true;
    try {
      xProductOutboundBean.getL5ItemListing(productSkuList, Collections.singletonList(PRODUCT_SKU),
          Collections.singletonList(PRODUCT_SKU), false, null);
    } catch (ApplicationRuntimeException ex) {
      isSuccess = false;
    } finally {
      Mockito.verify(xProductFeign)
          .getL5ItemList(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
              GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(), true,
              false, null, new ItemLevel4ListingWebRequest(productSkuList, Collections.singletonList(PRODUCT_SKU),
                  Collections.singletonList(PRODUCT_SKU)));
    }
    Assertions.assertFalse(isSuccess);
  }

  @Test
  public void updateItemListingExceptionTest() throws Exception {
    Mockito.when(xProductFeign
        .updateItemListing(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU,
            new ItemListingUpdateRequest(ProductType.REGULAR, new ArrayList<>())))
        .thenReturn(new GdnBaseRestResponse(null, null, false, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateItemListing(PRODUCT_SKU, ProductType.REGULAR, new ArrayList<>());
      });
    } finally {
      Mockito.verify(xProductFeign)
          .updateItemListing(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU,
              new ItemListingUpdateRequest(ProductType.REGULAR, new ArrayList<>()));
    }
  }

  @Test
  public void updateOff2OnActiveFlagByProductSkuTest() throws Exception {
    SimpleStringBooleanMapRequest request = new SimpleStringBooleanMapRequest(new HashMap<>());
    Map<String, Boolean> off2OnFLagByProductSkuMap = new HashMap<>();
    Mockito.when(xProductFeign.bulkUpdateOff2OnActiveFlagByProductSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), request))
        .thenReturn(new GdnRestSingleResponse<>(new SimpleListStringResponse(), DEFAULT_REQUEST_ID));
    xProductOutboundBean.updateOff2OnActiveFlagByProductSku(off2OnFLagByProductSkuMap);
    Mockito.verify(xProductFeign).bulkUpdateOff2OnActiveFlagByProductSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), request);
  }

  @Test
  public void updateOff2OnActiveFlagByProductSkuExceptionTest() throws Exception {
    SimpleStringBooleanMapRequest request = new SimpleStringBooleanMapRequest(new HashMap<>());
    Map<String, Boolean> off2OnFLagByProductSkuMap = new HashMap<>();
    Mockito.when(xProductFeign.bulkUpdateOff2OnActiveFlagByProductSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), request))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateOff2OnActiveFlagByProductSku(off2OnFLagByProductSkuMap);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .bulkUpdateOff2OnActiveFlagByProductSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
              Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), request);
    }
  }

  @Test
  public void generateProductScoreByProductSkuOrProductCodeTest() {
    Mockito.when(xProductFeign
        .generateProductScoreByProductSkuOrProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, PRODUCT_SKU,
            DEFAULT_PRODUCT_CODE)).thenReturn(new GdnRestSingleResponse<>(null,null, true, null, null));
    xProductOutboundBean.generateProductScoreByProductSkuOrProductCode(PRODUCT_SKU, DEFAULT_PRODUCT_CODE, false);
    Mockito.verify(xProductFeign)
        .generateProductScoreByProductSkuOrProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, PRODUCT_SKU,
            DEFAULT_PRODUCT_CODE);
  }

  @Test
  public void generateProductScoreByProductSkuOrProductCodeExceptionTest() {
    Mockito.when(xProductFeign.generateProductScoreByProductSkuOrProductCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, false, PRODUCT_SKU, DEFAULT_PRODUCT_CODE)).thenReturn(new GdnRestSingleResponse<>(null,null, true, null, null));
    xProductOutboundBean.generateProductScoreByProductSkuOrProductCode(PRODUCT_SKU, DEFAULT_PRODUCT_CODE, false);
    Mockito.verify(xProductFeign)
        .generateProductScoreByProductSkuOrProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, PRODUCT_SKU,
            DEFAULT_PRODUCT_CODE);
  }
  @Test
  public void generateProductScoreByProductSkuOrProductCodeSuccessFalseTest() {
    Mockito.when(xProductFeign
        .generateProductScoreByProductSkuOrProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, PRODUCT_SKU,
            DEFAULT_PRODUCT_CODE)).thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    xProductOutboundBean.generateProductScoreByProductSkuOrProductCode(PRODUCT_SKU, DEFAULT_PRODUCT_CODE, false);
    Mockito.verify(xProductFeign)
        .generateProductScoreByProductSkuOrProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, PRODUCT_SKU,
            DEFAULT_PRODUCT_CODE);
  }

  @Test
  public void generateProductScoreByProductSkuOrProductCodeWithUpdateCategoryTest() {
    Mockito.when(xProductFeign
        .generateProductScoreByProductSkuOrProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, PRODUCT_SKU,
            DEFAULT_PRODUCT_CODE)).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, null));
    xProductOutboundBean.generateProductScoreByProductSkuOrProductCode(PRODUCT_SKU, DEFAULT_PRODUCT_CODE, true);
    Mockito.verify(xProductFeign)
        .generateProductScoreByProductSkuOrProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, PRODUCT_SKU,
            DEFAULT_PRODUCT_CODE);
  }

  @Test
  public void generateProductScoreByProductSkuOrProductCodeSuccessFalseWithUpdateCategoryTest() {
    Mockito.when(xProductFeign
        .generateProductScoreByProductSkuOrProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, PRODUCT_SKU,
            DEFAULT_PRODUCT_CODE)).thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    xProductOutboundBean.generateProductScoreByProductSkuOrProductCode(PRODUCT_SKU, DEFAULT_PRODUCT_CODE, true);
    Mockito.verify(xProductFeign)
        .generateProductScoreByProductSkuOrProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true, PRODUCT_SKU,
            DEFAULT_PRODUCT_CODE);
  }

  @Test
  public void getItemPickupPointCodeResponseTest() throws Exception {
    Mockito.when(xProductFeign
        .getItemPickupPointCodeByProductSku(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, 0, 1, PRODUCT_SKU,
            false))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(1, 0, 10), DEFAULT_REQUEST_ID));
    Page<ItemPickupPointCodeResponse> responses =
        xProductOutboundBean.getItemPickupPointCodeResponse(PRODUCT_SKU, 0, 1, false);
    Mockito.verify(xProductFeign)
        .getItemPickupPointCodeByProductSku(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, 0, 1, PRODUCT_SKU ,
            false);
    Assertions.assertEquals(10, responses.getTotalElements());
  }

  @Test
  public void getItemPickupPointCodeResponseExceptionTest() throws Exception {
    Mockito.when(
        xProductFeign.getItemPickupPointCodeByProductSku(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, 0, 1, PRODUCT_SKU,
            false)).thenReturn(new GdnRestListResponse<>(null, null, false, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getItemPickupPointCodeResponse(PRODUCT_SKU, 0, 1, false);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getItemPickupPointCodeByProductSku(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, 0, 1, PRODUCT_SKU,
              false);
    }
  }

  @Test
  public void validateDuplicateProductBySellerSkuTest() throws Exception {
    Mockito.when(xProductFeign.validateDuplicateProductBySellerSku(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.DEFAULT_USERNAME, DEFAULT_MERCHANT_SKU, DEFAULT_MERCHANT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, DEFAULT_REQUEST_ID));
    DuplicateProductDetailsResponse duplicateProductDetailsResponse =
        xProductOutboundBean.validateDuplicateProductBySellerSku(DEFAULT_MERCHANT_SKU, DEFAULT_MERCHANT_CODE);
    Mockito.verify(xProductFeign).validateDuplicateProductBySellerSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        Constants.DEFAULT_USERNAME, DEFAULT_MERCHANT_SKU, DEFAULT_MERCHANT_CODE);
    Assertions.assertTrue(Objects.isNull(duplicateProductDetailsResponse));
  }

  @Test
  public void validateDuplicateProductBySellerSkuSuccessFalseAndErrorTest() throws Exception {
    Mockito.when(xProductFeign.validateDuplicateProductBySellerSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        Constants.DEFAULT_USERNAME, DEFAULT_MERCHANT_SKU, DEFAULT_MERCHANT_CODE)).thenReturn(
        new GdnRestSingleResponse<>("end pattern in group", null, false, null, DEFAULT_REQUEST_ID));
    DuplicateProductDetailsResponse duplicateProductDetailsResponse =
        xProductOutboundBean.validateDuplicateProductBySellerSku(DEFAULT_MERCHANT_SKU, DEFAULT_MERCHANT_CODE);
    Mockito.verify(xProductFeign).validateDuplicateProductBySellerSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        Constants.DEFAULT_USERNAME, DEFAULT_MERCHANT_SKU, DEFAULT_MERCHANT_CODE);
    Assertions.assertTrue(Objects.isNull(duplicateProductDetailsResponse));
  }

  @Test
  public void validateDuplicateProductBySellerSkuSuccessFalseTest() throws Exception {
    DuplicateProductDetailsResponse productDetailsResponse = new DuplicateProductDetailsResponse();
    Mockito.when(xProductFeign.validateDuplicateProductBySellerSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        Constants.DEFAULT_USERNAME, DEFAULT_MERCHANT_SKU, DEFAULT_MERCHANT_CODE)).thenReturn(
        new GdnRestSingleResponse<>(com.gdn.x.product.constants.ErrorMessages.PRODUCT_ALREADY_EXIST_WITH_THE_SELLER_SKU,
            null, false, productDetailsResponse, DEFAULT_REQUEST_ID));
    DuplicateProductDetailsResponse duplicateProductDetailsResponse =
        xProductOutboundBean.validateDuplicateProductBySellerSku(DEFAULT_MERCHANT_SKU, DEFAULT_MERCHANT_CODE);
    Mockito.verify(xProductFeign).validateDuplicateProductBySellerSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        Constants.DEFAULT_USERNAME, DEFAULT_MERCHANT_SKU, DEFAULT_MERCHANT_CODE);
    Assertions.assertTrue(Objects.nonNull(duplicateProductDetailsResponse));
  }

  @Test
  public void getPrdProductDetailByProductSkuOrProductCodeTest() throws Exception {
    ProductSkuAndProductCodeRequest productSkuAndProductCodeRequest = new ProductSkuAndProductCodeRequest();
    Mockito.when(xProductFeign.getPrdProductDetailByProductSkuOrProductCode(Mockito.any(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(), Mockito.anyString(), Mockito.any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, null));
    xProductOutboundBean.getPrdProductDetailByProductSkuOrProductCode(productSkuAndProductCodeRequest);
    Mockito.verify(xProductFeign)
        .getPrdProductDetailByProductSkuOrProductCode(Mockito.any(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void getPrdProductDetailByProductSkuOrProductCodeSuccessFalseTest() throws Exception {
    ProductSkuAndProductCodeRequest productSkuAndProductCodeRequest = new ProductSkuAndProductCodeRequest();
    Mockito.when(xProductFeign.getPrdProductDetailByProductSkuOrProductCode(Mockito.any(), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(), Mockito.anyString(), Mockito.any()))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null));
    xProductOutboundBean.getPrdProductDetailByProductSkuOrProductCode(productSkuAndProductCodeRequest);
    Mockito.verify(xProductFeign)
        .getPrdProductDetailByProductSkuOrProductCode(Mockito.any(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateContentChangeTest() throws Exception {
    Mockito.when(xProductFeign.updateContentChange(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU,
        true, false)).thenReturn(new GdnBaseRestResponse(true));
    xProductOutboundBean.updateContentChange(PRODUCT_SKU, true, false);
    Mockito.verify(xProductFeign).updateContentChange(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU,
        true, false);
  }

  @Test
  public void updateContentChangeExceptionTest() throws Exception {
    Mockito.when(xProductFeign.updateContentChange(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU,
        true, true)).thenReturn(new GdnBaseRestResponse(false));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      xProductOutboundBean.updateContentChange(PRODUCT_SKU, true, true);
    });
    try {
    } finally {
      Mockito.verify(xProductFeign).updateContentChange(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU,
          true, true);
    }
  }

  @Test
  public void getProductCountByTypeTest() throws Exception {
    Mockito.when(xProductFeign.getProductCountByType(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        TYPE, DEFAULT_MERCHANT_CODE)).thenReturn(new GdnRestSingleResponse<>(productCountResponse,
        GdnMandatoryRequestParameterUtil.getRequestId()));
    xProductOutboundBean.getProductCountByType(DEFAULT_MERCHANT_CODE, TYPE);
    Mockito.verify(xProductFeign).getProductCountByType(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        TYPE, DEFAULT_MERCHANT_CODE);
  }

  @Test
  public void getProductCountByTypeExceptionTest() throws Exception {
    Mockito.when(xProductFeign.getProductCountByType(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        TYPE, DEFAULT_MERCHANT_CODE)).thenReturn(new GdnRestSingleResponse<>(null, null, false, null, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getProductCountByType(DEFAULT_MERCHANT_CODE, TYPE);
      });
    } finally {
      Mockito.verify(xProductFeign).getProductCountByType(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          TYPE, DEFAULT_MERCHANT_CODE);
    }
  }

  @Test
  public void getProductAndSingleItemByItemSkuTest() {
    Mockito.when(xProductFeign
        .getProductAndSingleItemByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_ITEM_SKU, true, true)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new ProductAndItemsResponse(),
            GdnMandatoryRequestParameterUtil.getRequestId()));
    GdnRestSingleResponse response = xProductOutboundBean.getProductAndSingleItemByItemSku(DEFAULT_ITEM_SKU, true);
    Mockito.verify(xProductFeign)
        .getProductAndSingleItemByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_ITEM_SKU, true, true);
  }

  @Test
  public void getProductAndSingleItemByItemSkuExceptionTest() {
    Mockito.when(xProductFeign
        .getProductAndSingleItemByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_ITEM_SKU, false, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getProductAndSingleItemByItemSku(DEFAULT_ITEM_SKU, false);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getProductAndSingleItemByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_ITEM_SKU, false, true);
    }
  }

  @Test
  public void updateEditedProductTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    ProductRequest productRequestForEdit = new ProductRequest();
    productRequestForEdit.setPreOrder(preOrderDTO);
    productEditRequest.setProductRequest(productRequestForEdit);
    ArgumentCaptor<ProductEditRequest> captor = ArgumentCaptor.forClass(ProductEditRequest.class);
    Mockito.when(xProductFeign
        .updateEditedProduct(eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), any(ProductEditRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    xProductOutboundBean.updateEditedProduct(productEditRequest, false);
    Mockito.verify(xProductFeign)
        .updateEditedProduct(eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), captor.capture());
    Assertions.assertNotNull(captor.getValue().getProductRequest().getPreOrder());
    Assertions.assertTrue((Boolean) ReflectionTestUtils.getField(captor.getValue().getProductRequest().getPreOrder(), "convertToJKT"));
  }

  @Test
  public void updateEditedProductExceptionTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    ProductRequest productRequestForEdit = new ProductRequest();
    productRequestForEdit.setPreOrder(preOrderDTO);
    productEditRequest.setProductRequest(productRequestForEdit);
    ArgumentCaptor<ProductEditRequest> captor = ArgumentCaptor.forClass(ProductEditRequest.class);
    Mockito.when(xProductFeign
        .updateEditedProduct(eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), any(ProductEditRequest.class)))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateEditedProduct(productEditRequest, false);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .updateEditedProduct(eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
              eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
              eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), captor.capture());
      Assertions.assertNotNull(captor.getValue().getProductRequest().getPreOrder());
      Assertions.assertTrue((Boolean) ReflectionTestUtils.getField(captor.getValue().getProductRequest().getPreOrder(), "convertToJKT"));
    }
  }

  @Test
  public void getListOfImagesByItemSkusTest() {
    Mockito.when(xProductFeign
        .getListOfImagesByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), new SimpleSetStringRequest()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, null));
    xProductOutboundBean.getListOfImagesByItemSkus(new SimpleSetStringRequest());
    Mockito.verify(xProductFeign)
        .getListOfImagesByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), new SimpleSetStringRequest());
  }

  @Test
  public void getListOfImagesByItemSkusExceptionTest() {
    Mockito.when(xProductFeign
        .getListOfImagesByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), new SimpleSetStringRequest()))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getListOfImagesByItemSkus(new SimpleSetStringRequest());
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getListOfImagesByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), new SimpleSetStringRequest());
    }
  }

  @Test
  public void activateOnNeedCorrectionTest() throws Exception {
    Mockito.when(xProductFeign.activateOnNeedCorrection(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        new NeedCorrectionProductActivationRequest()))
        .thenReturn(new GdnRestSimpleResponse<>(DEFAULT_REQUEST_ID, new ActivateNeedRevisionResponse()));
    xProductOutboundBean.activateOnNeedCorrection(new NeedCorrectionProductActivationRequest());
    Mockito.verify(xProductFeign).activateOnNeedCorrection(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        new NeedCorrectionProductActivationRequest());
  }

  @Test
  public void activateOnNeedCorrectionExceptionTest() throws Exception {
    Mockito.when(xProductFeign.activateOnNeedCorrection(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        new NeedCorrectionProductActivationRequest()))
        .thenReturn(new GdnRestSimpleResponse<>(null, null, false, DEFAULT_REQUEST_ID, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.activateOnNeedCorrection(new NeedCorrectionProductActivationRequest());
      });
    } finally {
      Mockito.verify(xProductFeign).activateOnNeedCorrection(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          new NeedCorrectionProductActivationRequest());
    }
  }

  @Test
  public void getProductTypeByProductCodeTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, "");
    Mockito.when(xProductFeign.getProductDetailsByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(new ProductTypeResponse(), "REQUEST"));
    xProductOutboundBean.getProductTypeByProductCode(PRODUCT_SKU);
    Mockito.verify(xProductFeign).getProductDetailsByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
  }

  @Test
  public void getProductTypeByProductCodeExceptionTest() throws Exception {
    Mockito.when(xProductFeign.getProductDetailsByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU))
        .thenThrow(new ApplicationRuntimeException());
    xProductOutboundBean.getProductTypeByProductCode(PRODUCT_SKU);
    Mockito.verify(xProductFeign).getProductDetailsByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
  }

  @Test
  public void updateWholeSaleActivationFlagTest() {
    Mockito.when(xProductFeign.updateWholeSaleActivationFlag(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
        new SimpleListStringRequest(Arrays.asList(DEFAULT_ITEM_SKU)))).thenReturn(new GdnBaseRestResponse(true));
    xProductOutboundBean.updateWholeSaleActivationFlag(Arrays.asList(DEFAULT_ITEM_SKU), false);
    Mockito.verify(xProductFeign).updateWholeSaleActivationFlag(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
        new SimpleListStringRequest(Arrays.asList(DEFAULT_ITEM_SKU)));
  }

  @Test
  public void updateWholeSaleActivationFlagExceptionTest() {
    Mockito.when(xProductFeign.updateWholeSaleActivationFlag(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
        new SimpleListStringRequest(Arrays.asList(DEFAULT_ITEM_SKU)))).thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateWholeSaleActivationFlag(Arrays.asList(DEFAULT_ITEM_SKU), false);
      });
    } finally {
      Mockito.verify(xProductFeign).updateWholeSaleActivationFlag(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
          new SimpleListStringRequest(Arrays.asList(DEFAULT_ITEM_SKU)));
    }
  }

  @Test
  public void getPickupPointDetailsByListOfPickupPointCodesTest() {
    Mockito.when(xProductFeign.getBusinessPartnerPickupPointDetails(
      GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(),
      new SimpleListStringRequest(Arrays.asList(PICKUP_POINT_CODE)))).thenReturn(
      new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(businessPartnerPickupPointResponse), null, DEFAULT_REQUEST_ID));
    xProductOutboundBean.getPickupPointDetailsByListOfPickupPointCodes(Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(xProductFeign).getBusinessPartnerPickupPointDetails(
      GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(),
      new SimpleListStringRequest(Arrays.asList(PICKUP_POINT_CODE)));
  }

  @Test
  public void getPickupPointDetailsByListOfPickupPointCodesExceptionTest() {
    Mockito.when(xProductFeign.getBusinessPartnerPickupPointDetails(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        new SimpleListStringRequest(Arrays.asList(PICKUP_POINT_CODE))))
      .thenReturn(new GdnRestListResponse<>(null, null, false, DEFAULT_REQUEST_ID));
    try {
    } finally {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getPickupPointDetailsByListOfPickupPointCodes(
            Arrays.asList(PICKUP_POINT_CODE));
      });
      Mockito.verify(xProductFeign).getBusinessPartnerPickupPointDetails(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        new SimpleListStringRequest(Arrays.asList(PICKUP_POINT_CODE)));
    }
  }

  @Test
  public void getItemPickupPointCodeByItemSkusTest() {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> gdnRestListResponse = new GdnRestListResponse();
    gdnRestListResponse.setSuccess(true);
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_ITEM_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList = new ArrayList<>();
    itemSkuPickupPointCodeResponseList.add(itemSkuPickupPointCodeResponse);
    gdnRestListResponse.setContent(itemSkuPickupPointCodeResponseList);

    Mockito.when(xProductFeign
        .getItemPickupPointCodeByItemSkus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            new SimpleListStringRequest(Arrays.asList(DEFAULT_ITEM_SKU)))).thenReturn(gdnRestListResponse);

    SimpleListStringRequest itemSkusList = new SimpleListStringRequest();
    itemSkusList.setValue(Arrays.asList(DEFAULT_ITEM_SKU));
    xProductOutboundBean.getItemPickupPointCodeByItemSkus(itemSkusList);

    Mockito.verify(xProductFeign)
        .getItemPickupPointCodeByItemSkus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            new SimpleListStringRequest(Arrays.asList(DEFAULT_ITEM_SKU)));
  }

  @Test
  public void getItemPickupPointCodeByItemSkusExceptionTest() {
    GdnRestListResponse gdnRestListResponse = new GdnRestListResponse();
    gdnRestListResponse.setSuccess(false);
    Mockito.when(xProductFeign.getItemPickupPointCodeByItemSkus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        new SimpleListStringRequest(Arrays.asList(DEFAULT_ITEM_SKU)))).thenReturn(gdnRestListResponse);

    SimpleListStringRequest itemSkusList = new SimpleListStringRequest();
    itemSkusList.setValue(Arrays.asList(DEFAULT_ITEM_SKU));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getItemPickupPointCodeByItemSkus(itemSkusList);
      });
    } finally {
      Mockito.verify(xProductFeign).getItemPickupPointCodeByItemSkus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          new SimpleListStringRequest(Arrays.asList(DEFAULT_ITEM_SKU)));
    }
  }

  @Test
  public void getItemPickupPointCodeByItemSkusListEmptyTest() {
    GdnRestListResponse gdnRestListResponse = new GdnRestListResponse();
    gdnRestListResponse.setSuccess(false);
    Mockito.when(
        xProductFeign.getItemPickupPointCodeByItemSkus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            new SimpleListStringRequest(Arrays.asList(DEFAULT_ITEM_SKU)))).thenReturn(gdnRestListResponse);

    SimpleListStringRequest itemSkusList = new SimpleListStringRequest();
    itemSkusList.setValue(new ArrayList<>());
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      xProductOutboundBean.getItemPickupPointCodeByItemSkus(itemSkusList);
    });
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      xProductOutboundBean.getItemPickupPointCodeByItemSkus(itemSkusList);
    });
  }

  @Test
  public void getItemPickupSummary_exceptionTest() {
    Mockito.when(this.xProductFeign.findItemSummaryByItemSkuAndPickupPointCode(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), null,
        Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(new GdnRestListResponse<>(null, null, false, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getItemPickupPointSummary(
            Collections.singletonList(itemPickupPointRequest), null);
      });
    } finally {
      Mockito.verify(this.xProductFeign)
        .findItemSummaryByItemSkuAndPickupPointCode(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), null,
          Collections.singletonList(itemPickupPointRequest));
    }
  }

  @Test
  public void updateItemPickupPointListingTest() {
    Mockito.when(this.xProductFeign.updateItemPickupPointListing(
      eq(GdnMandatoryRequestParameterUtil.getStoreId()),
      eq(GdnMandatoryRequestParameterUtil.getChannelId()),
      eq(GdnMandatoryRequestParameterUtil.getClientId()),
      eq(GdnMandatoryRequestParameterUtil.getRequestId()),
      eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(PRODUCT_SKU),
      Mockito.any(ItemPickupPointListingUpdateRequest.class))).thenReturn(
      new GdnRestListResponse<>(null, null, true, new ArrayList<>(), null, DEFAULT_REQUEST_ID));
    xProductOutboundBean.updateItemPickupPointListing(PRODUCT_SKU, ProductType.BIG_PRODUCT,
      Collections.emptyList());
    Mockito.verify(this.xProductFeign).updateItemPickupPointListing(
      eq(GdnMandatoryRequestParameterUtil.getStoreId()),
      eq(GdnMandatoryRequestParameterUtil.getChannelId()),
      eq(GdnMandatoryRequestParameterUtil.getClientId()),
      eq(GdnMandatoryRequestParameterUtil.getRequestId()),
      eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(PRODUCT_SKU),
      Mockito.any(ItemPickupPointListingUpdateRequest.class));
  }

  @Test
  public void updateItemPickupPointListing_exceptionTest() {
    Mockito.when(
        this.xProductFeign.updateItemPickupPointListing(Mockito.any(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any())).thenThrow(NullPointerException.class);
    try {
      Assertions.assertThrows(NullPointerException.class, () -> {
        xProductOutboundBean.updateItemPickupPointListing(PRODUCT_SKU, ProductType.BIG_PRODUCT,
            Collections.emptyList());
      });
    } finally {
      Mockito.verify(this.xProductFeign).updateItemPickupPointListing(Mockito.any(), Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
    }
  }

  @Test
  public void updateMasterDataFieldsInProductTest() throws Exception {
    Mockito.when(xProductFeign.updateMasterDataFieldsInProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_SKU))
        .thenReturn(new GdnBaseRestResponse(true));
    xProductOutboundBean.updateMasterDataFieldsInProduct(PRODUCT_SKU);
    Mockito.verify(xProductFeign)
        .updateMasterDataFieldsInProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_SKU);
  }

  @Test
  public void updateMasterDataFieldsInProductExceptionTest() throws Exception {
    Mockito.when(xProductFeign.updateMasterDataFieldsInProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_SKU))
        .thenReturn(new GdnBaseRestResponse(false));
    xProductOutboundBean.updateMasterDataFieldsInProduct(PRODUCT_SKU);
    Mockito.verify(xProductFeign)
        .updateMasterDataFieldsInProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_SKU);
  }

  @Test
  public void addProductAndItemsTest() throws ApplicationException {
    ProductAndItemActivationRequest productAndItemActivationRequest = new ProductAndItemActivationRequest();
    Mockito.when(xProductFeign.addProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            productAndItemActivationRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new AddProductAndItemsResponse(), "REQUEST"));
    xProductOutboundBean.addProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        productAndItemActivationRequest);
    Mockito.verify(xProductFeign).addProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        productAndItemActivationRequest);
  }

  @Test
  public void addProductAndItemsSucessFalseTest() throws ApplicationException {
    ProductAndItemActivationRequest productAndItemActivationRequest = new ProductAndItemActivationRequest();
    Mockito.when(xProductFeign.addProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            productAndItemActivationRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, new AddProductAndItemsResponse(), "REQUEST"));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        xProductOutboundBean.addProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            productAndItemActivationRequest);
      });
    } finally {
      Mockito.verify(xProductFeign).addProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          productAndItemActivationRequest);
    }
  }

  @Test
  public void getItemPickupPointListTest() {
    ItemPickupPointListingRequest itemPickupPointListingRequest = new ItemPickupPointListingRequest();
    Mockito.when(xProductFeign.getItemPickupPointList(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), 0, 1,
        itemPickupPointListingRequest, "ALL")).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(1, 0, 1), DEFAULT_REQUEST_ID));
    xProductOutboundBean.getItemPickupPointList(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), 0, 1,
        itemPickupPointListingRequest);
    Mockito.verify(xProductFeign).getItemPickupPointList(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), 0, 1,
        itemPickupPointListingRequest, "ALL");
  }

  @Test
  public void getItemPickupPointListSuccessFalseTest() {
    ItemPickupPointListingRequest itemPickupPointListingRequest = new ItemPickupPointListingRequest();
    Mockito.when(xProductFeign.getItemPickupPointList(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), 0, 1,
        itemPickupPointListingRequest, "ALL")).thenReturn(
        new GdnRestListResponse<>(null, null, false, new ArrayList<>(), new PageMetaData(1, 0, 1), DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getItemPickupPointList(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), 0, 1,
            itemPickupPointListingRequest);
      });
    } finally {
      Mockito.verify(xProductFeign).getItemPickupPointList(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), 0, 1,
          itemPickupPointListingRequest, "ALL");
    }
  }

  @Test
  public void getItemPickupPointListNullTest() {
    ItemPickupPointListingRequest itemPickupPointListingRequest = new ItemPickupPointListingRequest();
    Mockito.when(xProductFeign.getItemPickupPointList(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), 0, 1,
            itemPickupPointListingRequest, "ALL"))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, new PageMetaData(1, 0, 1), DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getItemPickupPointList(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), 0, 1,
            itemPickupPointListingRequest);
      });
    } finally {
      Mockito.verify(xProductFeign).getItemPickupPointList(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), 0, 1,
          itemPickupPointListingRequest, "ALL");
    }
  }


  @Test
  public void getPickupPointDetailResponseTest() {
    Mockito.when(xProductFeign.getPickupPointDetailByCodes(
      GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(),
      new SimpleListStringRequest(Arrays.asList(PICKUP_POINT_CODE)))).thenReturn(
      new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new PickupPointDetailResponse()), null, DEFAULT_REQUEST_ID));
    xProductOutboundBean.getPickupPointDetailResponse(Arrays.asList(PICKUP_POINT_CODE), false);
    Mockito.verify(xProductFeign).getPickupPointDetailByCodes(
      GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(),
      new SimpleListStringRequest(Arrays.asList(PICKUP_POINT_CODE)));
  }

  @Test
  public void getPickupPointDetailResponseExceptionTest() {
    Mockito.when(xProductFeign.getPickupPointDetailByCodes(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        new SimpleListStringRequest(Arrays.asList(PICKUP_POINT_CODE))))
      .thenReturn(new GdnRestListResponse<>(null, null, false, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getPickupPointDetailResponse(Arrays.asList(PICKUP_POINT_CODE), false);
      });
    } finally {
      Mockito.verify(xProductFeign).getPickupPointDetailByCodes(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        new SimpleListStringRequest(Arrays.asList(PICKUP_POINT_CODE)));
    }
  }

  @Test
  public void getItemPickupPointSummaryTest() {
    Mockito.when(
      xProductFeign.getItemPickupPointSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PAGE, SIZE, Constants.ALL,
        itemPickupPointSummaryRequest)).thenReturn(
      new GdnRestListResponse<>(null, null, true, Collections.emptyList(), new PageMetaData(PAGE,
        SIZE, SIZE), DEFAULT_REQUEST_ID));
    xProductOutboundBean.getItemPickupPointSummary(PAGE, SIZE, itemPickupPointSummaryRequest);
    Mockito.verify(xProductFeign)
      .getItemPickupPointSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PAGE, SIZE, Constants.ALL, itemPickupPointSummaryRequest);
  }

  @Test
  public void getItemPickupPointSummaryExceptionTest() {
    Mockito.when(
        xProductFeign.getItemPickupPointSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), PAGE, SIZE,  Constants.ALL, itemPickupPointSummaryRequest))
      .thenReturn(new GdnRestListResponse<>(null, null, false, Collections.emptyList(), null,
        DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getItemPickupPointSummary(PAGE, SIZE, itemPickupPointSummaryRequest);
      });
    } finally {
      Mockito.verify(xProductFeign)
        .getItemPickupPointSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), PAGE, SIZE,  Constants.ALL,
          itemPickupPointSummaryRequest);
    }
  }


  @Test
  public void filterSummaryL3Test() throws ApplicationException {
    ProductSummaryRequest productAndItemActivationRequest = new ProductSummaryRequest();
    Mockito.when(xProductFeign.getFilterSummaryL3(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        PAGE, SIZE, productAndItemActivationRequest))
      .thenReturn(new GdnRestListResponse<>(null, null, true, REQUEST_ID));
    xProductOutboundBean.filterSummaryL3(PAGE, SIZE, productAndItemActivationRequest);
    Mockito.verify(xProductFeign).getFilterSummaryL3(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
      PAGE, SIZE, productAndItemActivationRequest);
  }

  @Test
  public void getItemNameByItemSkusTest() throws ApplicationException {
    SimpleMapStringResponse response = new SimpleMapStringResponse();
    SimpleListStringRequest itemSkus = new SimpleListStringRequest();
    Mockito.when(xProductFeign.getItemNameByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            includeMarkForDelete, itemSkus))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, response, REQUEST_ID));
    xProductOutboundBean.getItemNameByItemSkus(itemSkus, includeMarkForDelete);
    Mockito.verify(xProductFeign).getItemNameByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        includeMarkForDelete, itemSkus);
  }


  @Test
  public void getItemNameByItemSkusSucessFalseTest() throws ApplicationException {
    SimpleMapStringResponse response = new SimpleMapStringResponse();
    SimpleListStringRequest itemSkus = new SimpleListStringRequest();
    Mockito.when(xProductFeign.getItemNameByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            includeMarkForDelete, itemSkus))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, response, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        xProductOutboundBean.getItemNameByItemSkus(itemSkus, includeMarkForDelete);
      });
    } finally {
      Mockito.verify(xProductFeign).getItemNameByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          includeMarkForDelete, itemSkus);
    }
  }
  @Test
  public void filterSummaryL3SucessFalseTest() throws ApplicationException {
    ProductSummaryRequest productAndItemActivationRequest = new ProductSummaryRequest();
    Mockito.when(xProductFeign.getFilterSummaryL3(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        PAGE, SIZE, productAndItemActivationRequest))
      .thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        xProductOutboundBean.filterSummaryL3(PAGE, SIZE, productAndItemActivationRequest);
      });
    } finally {
      Mockito.verify(xProductFeign).getFilterSummaryL3(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        PAGE, SIZE, productAndItemActivationRequest);
    }
  }

  @Test
  public void getProductSummaryTest() throws ApplicationException {
    ProductSummaryRequestV2 productAndItemActivationRequest = new ProductSummaryRequestV2();
    Mockito.when(xProductFeign.getProductSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(), PAGE, SIZE, productAndItemActivationRequest))
      .thenReturn(new GdnRestListResponse<>(null, null, true, REQUEST_ID));
    xProductOutboundBean.getProductSummary(PAGE, SIZE, productAndItemActivationRequest);
    Mockito.verify(xProductFeign).getProductSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(), PAGE, SIZE, productAndItemActivationRequest);
  }

  @Test
  public void getProductSummarySuccessFalseTest() throws ApplicationException {
    ProductSummaryRequestV2 productAndItemActivationRequest = new ProductSummaryRequestV2();
    Mockito.when(xProductFeign.getProductSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(), PAGE, SIZE, productAndItemActivationRequest))
      .thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        xProductOutboundBean.getProductSummary(PAGE, SIZE, productAndItemActivationRequest);
      });
    } finally {
      Mockito.verify(xProductFeign).getProductSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PAGE, SIZE,
        productAndItemActivationRequest);
    }
  }

  @Test
  public void updateItemPickupPointsNoUpdateTest() {
    EditItemResponse editItemResponse =
        xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null, null, null, null, null,
            false, null, false, null, false, null, new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
    Assertions.assertEquals(editItemResponse, new EditItemResponse());
  }

  @Test
  public void updateItemPickupPoints1Test() {
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(DEFAULT_ITEM_SKU);
    Mockito.when(xProductFeign.updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any())).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, null));
    xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null,
        Collections.singletonList(itemPickupPointQuickEditRequest), null, null, null, false, null, false, true, true,
        new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
    Mockito.verify(xProductFeign).updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void updateItemPickupPointsFbbFalseAndCncFalseTest() {
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(DEFAULT_ITEM_SKU);
    Mockito.when(xProductFeign.updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any())).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, null));
    xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null,
        Collections.singletonList(itemPickupPointQuickEditRequest), null, null, null, false, null, false, null, false,
        new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
    Mockito.verify(xProductFeign).updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void updateItemPickupPointsFbbFalseAndCncTrueTest() {
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(DEFAULT_ITEM_SKU);
    Mockito.when(xProductFeign.updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any())).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, null));
    xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null,
        Collections.singletonList(itemPickupPointQuickEditRequest), null, null, null, false, true, true, false, false,
        new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
    Mockito.verify(xProductFeign).updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void updateItemPickupPoints5Test() {
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(DEFAULT_ITEM_SKU);
    Mockito.when(xProductFeign.updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any())).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, null));
    xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null,
        null, null, null, true, true, null, false, null, false, new AddDeleteVariantRequest(),
        new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
    Mockito.verify(xProductFeign).updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void updateItemPickupPoints2Test() {
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(DEFAULT_ITEM_SKU);
    Mockito.when(xProductFeign.updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any())).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, null));
    xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null,
        null, Collections.singletonList(itemPickupPointQuickEditRequest), null, null, false, null, false, null, false,
        new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
    Mockito.verify(xProductFeign).updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void updateItemPickPoint3Test() {
    ItemPickupPointDeleteRequest itemPickupPointDeleteRequest = new ItemPickupPointDeleteRequest();
    itemPickupPointDeleteRequest.setItemSku(DEFAULT_ITEM_SKU);
    Mockito.when(xProductFeign.updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any())).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, null));
    xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null,
        null, null, Collections.singletonList(itemPickupPointDeleteRequest), null, false, null, false, null, false,
        new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
    Mockito.verify(xProductFeign).updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void updateItemPickPoint5Test() {
    ItemPickupPointDeleteRequest itemPickupPointDeleteRequest = new ItemPickupPointDeleteRequest();
    itemPickupPointDeleteRequest.setItemSku(DEFAULT_ITEM_SKU);
    Mockito.when(xProductFeign.updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any())).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, null));
    xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null,
        null, null, null, null, false, false, true, null, false, new AddDeleteVariantRequest(),
        new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
    Mockito.verify(xProductFeign).updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void updateItemPickPoint4Test() {
    ItemPickupPointDeleteRequest itemPickupPointDeleteRequest = new ItemPickupPointDeleteRequest();
    itemPickupPointDeleteRequest.setItemSku(DEFAULT_ITEM_SKU);
    Mockito.when(xProductFeign.updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any())).thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      Assertions.assertThrows(Exception.class, () -> {
        xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null, null, null,
            Collections.singletonList(itemPickupPointDeleteRequest), null, false, null, false, null, false,
            new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
      });
    } finally {
      Mockito.verify(xProductFeign).updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
          eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
          Mockito.any());
    }
  }

  @Test
  public void updateItemPickPointWithValidationErrorCategoryTest() {
    ItemPickupPointDeleteRequest itemPickupPointDeleteRequest = new ItemPickupPointDeleteRequest();
    itemPickupPointDeleteRequest.setItemSku(DEFAULT_ITEM_SKU);
    Mockito.when(xProductFeign.updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
        Mockito.any()))
        .thenReturn(new GdnRestSingleResponse<>("Can not process invalid input data :Failed to change the product price because it's already registered on another promo",
            ErrorCategory.VALIDATION.getCode(), false, null, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null, null, null,
            Collections.singletonList(itemPickupPointDeleteRequest), null, false, null, false, null, false,
            new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
      });
    } finally {
      Mockito.verify(xProductFeign).updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
          eq(GdnMandatoryRequestParameterUtil.getClientId()), eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(),
          Mockito.any());
    }
  }

  @Test
  public void getItemPickupPointsByItemSkuTest() throws ApplicationException {
    itemRequestV2.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU));
    Mockito.when(xProductFeign.getItemPickupPointsByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PAGE, 1, itemRequestV2))
        .thenReturn(new GdnRestListResponse<>(null, null, true, REQUEST_ID));
    xProductOutboundBean.getItemPickupPointsByItemSku(itemRequestV2);
    Mockito.verify(xProductFeign).getItemPickupPointsByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PAGE, 1, itemRequestV2);
  }

  @Test
  public void getItemPickupPointsByItemSkuSuccessFalseTest() throws ApplicationException {
    itemRequestV2.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU));
    Mockito.when(xProductFeign.getItemPickupPointsByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PAGE, 1, itemRequestV2))
        .thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        xProductOutboundBean.getItemPickupPointsByItemSku(itemRequestV2);
      });
    } finally {
      Mockito.verify(xProductFeign).getItemPickupPointsByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PAGE, 1, itemRequestV2);
    }
  }

  @Test
  public void updateCncActivationFlagTest() throws Exception {
    Mockito.when(xProductFeign.updateCncActivationFlag(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), new SimpleListStringRequest(
            Arrays.asList(DEFAULT_ITEM_SKU)))).thenReturn(new GdnBaseRestResponse(true));
    xProductOutboundBean.updateCncActivationFlag(Arrays.asList(DEFAULT_ITEM_SKU));
    Mockito.verify(xProductFeign).updateCncActivationFlag(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        new SimpleListStringRequest(Arrays.asList(DEFAULT_ITEM_SKU)));
  }

  @Test
  public void updateCncActivationFlagExceptionTest() throws Exception {
    Mockito.when(xProductFeign.updateCncActivationFlag(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), new SimpleListStringRequest(
            Arrays.asList(DEFAULT_ITEM_SKU)))).thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateCncActivationFlag(Arrays.asList(DEFAULT_ITEM_SKU));
      });
    } finally {
      Mockito.verify(xProductFeign).updateCncActivationFlag(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          new SimpleListStringRequest(Arrays.asList(DEFAULT_ITEM_SKU)));
    }
  }

  @Test
  public void createFbbPickupPointTest() throws Exception {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = new CreateFbbPickupPointRequest();
    GdnRestSingleResponse<CreateFbbPickupPointResponse> response =
        new GdnRestSingleResponse<>(new CreateFbbPickupPointResponse(), DEFAULT_REQUEST_ID);
    Mockito.when(xProductFeign.createFbbPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        createFbbPickupPointRequest)).thenReturn(response);
    xProductOutboundBean.createFbbPickupPoint(createFbbPickupPointRequest);
    Mockito.verify(xProductFeign).createFbbPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        createFbbPickupPointRequest);
  }

  @Test
  public void createFbbPickupPointExceptionTest() throws Exception {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = new CreateFbbPickupPointRequest();
    GdnRestSingleResponse<CreateFbbPickupPointResponse> response =
        new GdnRestSingleResponse<>(null, null, false, null, null);
    Mockito.when(xProductFeign.createFbbPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        createFbbPickupPointRequest)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.createFbbPickupPoint(createFbbPickupPointRequest);
      });
    } finally {
      Mockito.verify(xProductFeign).createFbbPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          createFbbPickupPointRequest);
    }
  }

  @Test
  public void deleteActiveItemPickupPointByPickupPointCodeTest() throws Exception {
    GdnRestListResponse<DeleteItemPickupPointResponse> response = new GdnRestListResponse<>();
    response.setContent(deletePPCodeResponse);
    response.setSuccess(Boolean.TRUE);

    Mockito.when(xProductFeign
      .deleteActiveItemPickupPointByPickupPointCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), deleteItemPickupPointRequest))
      .thenReturn(response);
    xProductOutboundBean.deleteActiveItemPickupPointByPickupPointCode(deleteItemPickupPointRequest);
    Mockito.verify(xProductFeign)
      .deleteActiveItemPickupPointByPickupPointCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), deleteItemPickupPointRequest);
  }

  @Test
  public void deleteActiveItemPickupPointByPickupPointCodeExceptionTest() throws Exception {
    GdnRestListResponse<DeleteItemPickupPointResponse> response = new GdnRestListResponse<>();
    response.setContent(deletePPCodeResponse);
    response.setSuccess(Boolean.FALSE);

    Mockito.when(xProductFeign
      .deleteActiveItemPickupPointByPickupPointCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), deleteItemPickupPointRequest))
      .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.deleteActiveItemPickupPointByPickupPointCode(deleteItemPickupPointRequest);
      });
    } finally {
      Mockito.verify(xProductFeign)
        .deleteActiveItemPickupPointByPickupPointCode(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), deleteItemPickupPointRequest);
    }
  }

  @Test
  public void getMinAndMaxOfferPriceTest() throws Exception {
    GdnRestSimpleResponse<MinMaxItemPriceResponse> response = new GdnRestSimpleResponse<>();
    response.setSuccess(Boolean.TRUE);
    Mockito.when(xProductFeign.getMinAndMaxOfferPrice(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            PRODUCT_CODE)).thenReturn(response);
    xProductOutboundBean.getMinAndMaxOfferPrice(PRODUCT_CODE);
    Mockito.verify(xProductFeign).getMinAndMaxOfferPrice(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_CODE);
  }

  @Test
  public void getMinAndMaxOfferPriceExceptionTest() throws Exception {
    GdnRestSimpleResponse<MinMaxItemPriceResponse> response = new GdnRestSimpleResponse<>();
    response.setSuccess(Boolean.FALSE);
    Mockito.when(xProductFeign.getMinAndMaxOfferPrice(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        PRODUCT_CODE)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getMinAndMaxOfferPrice(PRODUCT_CODE);
      });
    } finally {
      Mockito.verify(xProductFeign).getMinAndMaxOfferPrice(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_CODE);
    }
  }

  @Test
  public void findProductAndItemByItemSkuAndPickupPointCodeTest() {
    Mockito.when(xProductFeign.findProductAndItemByItemSkuAndPickupPointCode(
        GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), "ALL", Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(new GdnRestListResponse<>(null, null, true, REQUEST_ID));
    xProductOutboundBean.findProductAndItemByItemSkuAndPickupPointCode(
      Collections.singletonList(itemPickupPointRequest));
    Mockito.verify(xProductFeign)
      .findProductAndItemByItemSkuAndPickupPointCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), "ALL", Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void findProductAndItemByItemSkuAndPickupPointCodeExceptionTest() {
    Mockito.when(xProductFeign.findProductAndItemByItemSkuAndPickupPointCode(
        GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), "ALL", Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.findProductAndItemByItemSkuAndPickupPointCode(
            Collections.singletonList(itemPickupPointRequest));
      });
    } finally {
      Mockito.verify(xProductFeign).findProductAndItemByItemSkuAndPickupPointCode(
        GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), "ALL", Collections.singletonList(itemPickupPointRequest));
    }
  }

  @Test
  public void updateItemPickPointWithFbbValidationErrorCategoryTest() {
    EditItemResponse editItemResponse = new EditItemResponse();
    GdnRestSingleResponse<EditItemResponse> response = new GdnRestSingleResponse<>();
    editItemResponse.setApiErrorCode(ApiErrorCode.FBB_PICKUP_POINT_ALREADY_EXISTS);
    response.setValue(editItemResponse);
    ItemPickupPointDeleteRequest itemPickupPointDeleteRequest = new ItemPickupPointDeleteRequest();
    itemPickupPointDeleteRequest.setItemSku(DEFAULT_ITEM_SKU);
    Mockito.when(
      xProductFeign.updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
          Mockito.any(), Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null, null, null,
            Collections.singletonList(itemPickupPointDeleteRequest), null, false, null, false, null, false,
            new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
      });
    } finally {
      Mockito.verify(xProductFeign)
        .updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
          eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.any(), Mockito.any());
    }
  }
  @Test
  public void updateItemPickPointWithNullEditResponseTest() {
    EditItemResponse editItemResponse = new EditItemResponse();
    GdnRestSingleResponse<EditItemResponse> response = new GdnRestSingleResponse<>();
    editItemResponse.setApiErrorCode(null);
    response.setValue(editItemResponse);
    ItemPickupPointDeleteRequest itemPickupPointDeleteRequest = new ItemPickupPointDeleteRequest();
    itemPickupPointDeleteRequest.setItemSku(DEFAULT_ITEM_SKU);
    Mockito.when(
      xProductFeign.updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
          Mockito.any(), Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateItemPickupPoints(Constants.STORE_ID, PRODUCT_SKU, null, null, null,
            Collections.singletonList(itemPickupPointDeleteRequest), null, false, null, false, null, false,
            new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO());
      });
    } finally {
      Mockito.verify(xProductFeign)
        .updateItemPickupPoints(eq(Constants.STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
          eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void findItemBasicDetailsByProductSkuTest() {
    Mockito.when(xProductFeign.getItemBasicDetailsByProductSku(
            GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(null, null, true, REQUEST_ID));
    xProductOutboundBean.findItemBasicDetailsByProductSku(PRODUCT_SKU);
    Mockito.verify(xProductFeign)
        .getItemBasicDetailsByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
  }

  @Test
  public void findItemBasicDetailsByProductSkuExceptionTest() {
    Mockito.when(xProductFeign.getItemBasicDetailsByProductSku(
            GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.findItemBasicDetailsByProductSku(PRODUCT_SKU);
      });
    } finally {
      Mockito.verify(xProductFeign).getItemBasicDetailsByProductSku(
          GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
    }
  }


  @Test
  public void getBasicProductInfoTest() {
    Mockito.when(xProductFeign.basicProductInfo(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new BasicProductResponse(), REQUEST_ID));
    xProductOutboundBean.getBasicProductInfo(PRODUCT_SKU);
    Mockito.verify(xProductFeign).basicProductInfo(
        GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
  }

  @Test
  public void getBasicProductInfoExceptionTest() {
    Mockito.when(xProductFeign.basicProductInfo(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, new BasicProductResponse(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getBasicProductInfo(PRODUCT_SKU);
      });
    } finally {
      Mockito.verify(xProductFeign).basicProductInfo(
          GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
    }
  }

  @Test
  public void getBasicProductInfoV2Test() {
    Mockito.when(
            xProductFeign.getBasicProductInfo(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new BasicProductResponse(), REQUEST_ID));
    xProductOutboundBean.getBasicProductInfoV2(PRODUCT_SKU);
    Mockito.verify(xProductFeign)
        .getBasicProductInfo(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);

  }

  @Test
  public void updateEditedProductAndItemPickupPointTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    GdnRestSingleResponse<CombinedEditItemResponse> response =
      new GdnRestSingleResponse<>(null, null, true, new CombinedEditItemResponse(),
        GdnMandatoryRequestParameterUtil.getRequestId());
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    ProductEditRequest productEditRequestForUpdate = new ProductEditRequest();
    ProductRequest productRequestForUpdate = new ProductRequest();
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    productRequestForUpdate.setPreOrder(preOrderDTO);
    productEditRequestForUpdate.setProductRequest(productRequestForUpdate);
    productDetailPageEditRequest.setProductEditRequest(productEditRequestForUpdate);
    ArgumentCaptor<ProductDetailPageEditRequest> captor = ArgumentCaptor.forClass(ProductDetailPageEditRequest.class);
    Mockito.when(xProductFeign.updateEditedProductAndItemPickupPoint(
      eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
      eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
      eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(true), eq(PRODUCT_SKU),
      any(ProductDetailPageEditRequest.class))).thenReturn(response);
    xProductOutboundBean.updateEditedProductAndItemPickupPoint(PRODUCT_SKU, true,
      productDetailPageEditRequest);
    Mockito.verify(xProductFeign)
      .updateEditedProductAndItemPickupPoint(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(true), eq(PRODUCT_SKU),
        captor.capture());
    Assertions.assertNotNull(captor.getValue().getProductEditRequest().getProductRequest().getPreOrder());
    Assertions.assertTrue((Boolean) ReflectionTestUtils.getField(captor.getValue().getProductEditRequest().getProductRequest().getPreOrder(), "convertToJKT"));
  }

  @Test
  public void updateEditedProductAndItemPickupPointExceptionTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    GdnRestSingleResponse<CombinedEditItemResponse> response =
      new GdnRestSingleResponse<>(null, null, false, new CombinedEditItemResponse(),
        GdnMandatoryRequestParameterUtil.getRequestId());
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    ProductEditRequest productEditRequestForUpdate = new ProductEditRequest();
    ProductRequest productRequestForUpdate = new ProductRequest();
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    productRequestForUpdate.setPreOrder(preOrderDTO);
    productEditRequestForUpdate.setProductRequest(productRequestForUpdate);
    productDetailPageEditRequest.setProductEditRequest(productEditRequestForUpdate);
    ArgumentCaptor<ProductDetailPageEditRequest> captor = ArgumentCaptor.forClass(ProductDetailPageEditRequest.class);
    Mockito.when(xProductFeign.updateEditedProductAndItemPickupPoint(
      eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
      eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
      eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(true), eq(PRODUCT_SKU),
      any(ProductDetailPageEditRequest.class))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateEditedProductAndItemPickupPoint(PRODUCT_SKU, true, productDetailPageEditRequest);
      });
    } finally {
      Mockito.verify(xProductFeign)
        .updateEditedProductAndItemPickupPoint(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
          eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
          eq(GdnMandatoryRequestParameterUtil.getRequestId()),
          eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(true), eq(PRODUCT_SKU),
          captor.capture());
      Assertions.assertNotNull(captor.getValue().getProductEditRequest().getProductRequest().getPreOrder());
      Assertions.assertTrue((Boolean) ReflectionTestUtils.getField(captor.getValue().getProductEditRequest().getProductRequest().getPreOrder(), "convertToJKT"));
    }
  }

  @Test
  public void getProductBasicDetailsTest() {
    Mockito.when(xProductFeign.getProductBasicDetails(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
      Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)))).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 1, 1), REQUEST_ID));

    xProductOutboundBean.getProductBasicDetails(Arrays.asList(PRODUCT_SKU));

    Mockito.verify(xProductFeign).getProductBasicDetails(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
      Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void getProductBasicDetailsErrorTest() {
    Mockito.when(xProductFeign.getProductBasicDetails(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
      Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)))).thenReturn(
        new GdnRestListResponse<>(null, null, false, new ArrayList<>(), new PageMetaData(0, 1, 1), REQUEST_ID));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getProductBasicDetails(Arrays.asList(PRODUCT_SKU));
      });
    } finally {
      Mockito.verify(xProductFeign).getProductBasicDetails(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
    }
  }

  @Test
  public void updateEditedProductAndItemPickupPointExceptionValidationTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    GdnRestSingleResponse<CombinedEditItemResponse> response =
        new GdnRestSingleResponse<>(ErrorMessages.PRODUCT_ALREADY_EXIST_WITH_THE_SELLER_SKU,
            ErrorCategory.VALIDATION.name(), false, new CombinedEditItemResponse(),
            GdnMandatoryRequestParameterUtil.getRequestId());
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    ProductEditRequest productEditRequestForUpdate = new ProductEditRequest();
    ProductRequest productRequestForUpdate = new ProductRequest();
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    productRequestForUpdate.setPreOrder(preOrderDTO);
    productEditRequestForUpdate.setProductRequest(productRequestForUpdate);
    productDetailPageEditRequest.setProductEditRequest(productEditRequestForUpdate);
    ArgumentCaptor<ProductDetailPageEditRequest> captor = ArgumentCaptor.forClass(ProductDetailPageEditRequest.class);
    Mockito.when(xProductFeign.updateEditedProductAndItemPickupPoint(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(true), eq(PRODUCT_SKU),
        any(ProductDetailPageEditRequest.class))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateEditedProductAndItemPickupPoint(PRODUCT_SKU, true,
            productDetailPageEditRequest);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .updateEditedProductAndItemPickupPoint(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
              eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
              eq(GdnMandatoryRequestParameterUtil.getRequestId()),
              eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(true), eq(PRODUCT_SKU),
              captor.capture());
      Assertions.assertNotNull(captor.getValue().getProductEditRequest().getProductRequest().getPreOrder());
      Assertions.assertTrue((Boolean) ReflectionTestUtils.getField(captor.getValue().getProductEditRequest().getProductRequest().getPreOrder(), "convertToJKT"));
    }
  }

  @Test
  public void updateEditedProductAndItemPickupPointExceptionApiErrorCodeTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    GdnRestSingleResponse<CombinedEditItemResponse> response =
        new GdnRestSingleResponse<>(ErrorMessages.PRODUCT_ALREADY_EXIST_WITH_THE_SELLER_SKU,
            ErrorCategory.UNSPECIFIED.name(), false,
            CombinedEditItemResponse.builder().apiErrorCode(ApiErrorCode.ITEM_IS_SUSPENDED).build(),
            GdnMandatoryRequestParameterUtil.getRequestId());
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    ProductEditRequest productEditRequestForUpdate = new ProductEditRequest();
    ProductRequest productRequestForUpdate = new ProductRequest();
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    productRequestForUpdate.setPreOrder(preOrderDTO);
    productEditRequestForUpdate.setProductRequest(productRequestForUpdate);
    productDetailPageEditRequest.setProductEditRequest(productEditRequestForUpdate);
    ArgumentCaptor<ProductDetailPageEditRequest> captor = ArgumentCaptor.forClass(ProductDetailPageEditRequest.class);
    Mockito.when(xProductFeign.updateEditedProductAndItemPickupPoint(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(true), eq(PRODUCT_SKU), any(ProductDetailPageEditRequest.class)))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.updateEditedProductAndItemPickupPoint(PRODUCT_SKU, true, productDetailPageEditRequest);
      });
    } finally {
      Mockito.verify(xProductFeign).updateEditedProductAndItemPickupPoint(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
          eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
          eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(true), eq(PRODUCT_SKU), captor.capture());
      Assertions.assertNotNull(captor.getValue().getProductEditRequest().getProductRequest().getPreOrder());
      Assertions.assertTrue((Boolean) ReflectionTestUtils.getField(captor.getValue().getProductEditRequest().getProductRequest().getPreOrder(), "convertToJKT"));
    }
  }

  @Test
  public void updateItemPickupPointsCombinedNoUpdateTest() {
    xProductOutboundBean.updateItemPickupPointsForCombinedEdit(Constants.STORE_ID, PRODUCT_SKU, null, null, null, null,
        null, false, null, false, null, false, null, new ProductVariantUpdateRequest(), new EditFlagChangesDTO(),
        editProductResponse.getProductDetailEditDTO());
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void updateItemPickupPointsCombined1Test() {
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(DEFAULT_ITEM_SKU);
    xProductOutboundBean.updateItemPickupPointsForCombinedEdit(Constants.STORE_ID, PRODUCT_SKU, null,
        Collections.singletonList(itemPickupPointQuickEditRequest), null, null, null, false, null, false, true, true,
        new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO(),
        editProductResponse.getProductDetailEditDTO());
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void updateItemPickupPointsCombinedFbbFalseAndCncFalseTest() {
    editProductResponse.getProductDetailEditDTO().getProductDetailEditRequestForXProduct()
        .setEditChangeType(EditChangeType.CONTENT);
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(DEFAULT_ITEM_SKU);
    xProductOutboundBean.updateItemPickupPointsForCombinedEdit(Constants.STORE_ID, PRODUCT_SKU, null,
        Collections.singletonList(itemPickupPointQuickEditRequest), null, null, null, false, null, false, null, false,
        new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO(),
        editProductResponse.getProductDetailEditDTO());
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void updateItemPickupPointsCombinedFbbFalseAndCncTrueTest() {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productVariantUpdateRequest.setProductBundleRecipe(Arrays.asList(
        ProductBundleRecipeRequest.builder().itemSku(DEFAULT_ITEM_SKU).bundleRecipe(new HashSet<>()).build()));
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(DEFAULT_ITEM_SKU);
    xProductOutboundBean.updateItemPickupPointsForCombinedEdit(Constants.STORE_ID, PRODUCT_SKU, null,
        Collections.singletonList(itemPickupPointQuickEditRequest), null, null, null, false, true, true, false, false,
        new AddDeleteVariantRequest(), productVariantUpdateRequest, new EditFlagChangesDTO(),
        editProductResponse.getProductDetailEditDTO());
    Set<BundleRecipeRequest> bundleRecipeRequestSet =
        editProductResponse.getProductDetailEditDTO().getProductDetailEditRequestForXProduct()
            .getItemPickupPointUpdateRequest().getBundleRecipesRequests();
    Assertions.assertNotNull(bundleRecipeRequestSet);
    Assertions.assertEquals(DEFAULT_ITEM_SKU,
        bundleRecipeRequestSet.stream().map(BundleRecipeRequest::getItemSku).findFirst().orElse(null));
  }

  @Test
  public void updateItemPickupPointsCombined5Test() {
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(DEFAULT_ITEM_SKU);
    xProductOutboundBean.updateItemPickupPointsForCombinedEdit(Constants.STORE_ID, PRODUCT_SKU, null, null, null, null,
        true, true, null, false, null, false, new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(),
        new EditFlagChangesDTO(), editProductResponse.getProductDetailEditDTO());
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void updateItemPickupPointsCombined2Test() {
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(DEFAULT_ITEM_SKU);
    xProductOutboundBean.updateItemPickupPointsForCombinedEdit(Constants.STORE_ID, PRODUCT_SKU, null, null,
        Collections.singletonList(itemPickupPointQuickEditRequest), null, null, false, null, false, null, false,
        new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO(),
        editProductResponse.getProductDetailEditDTO());
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void updateItemPickPointCombined3Test() {
    ItemPickupPointDeleteRequest itemPickupPointDeleteRequest = new ItemPickupPointDeleteRequest();
    itemPickupPointDeleteRequest.setItemSku(DEFAULT_ITEM_SKU);
    xProductOutboundBean.updateItemPickupPointsForCombinedEdit(Constants.STORE_ID, PRODUCT_SKU, null, null, null,
        Collections.singletonList(itemPickupPointDeleteRequest), null, false, null, false, null, false,
        new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(), new EditFlagChangesDTO(),
        editProductResponse.getProductDetailEditDTO());
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void updateItemPickPointCombined5Test() {
    ItemPickupPointDeleteRequest itemPickupPointDeleteRequest = new ItemPickupPointDeleteRequest();
    itemPickupPointDeleteRequest.setItemSku(DEFAULT_ITEM_SKU);
    xProductOutboundBean.updateItemPickupPointsForCombinedEdit(Constants.STORE_ID, PRODUCT_SKU, null, null, null, null,
        null, false, false, true, null, false, new AddDeleteVariantRequest(), new ProductVariantUpdateRequest(),
        new EditFlagChangesDTO(), editProductResponse.getProductDetailEditDTO());
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void getItemBasicDetailsTest() {
    Mockito.when(xProductFeign.getItemBasicDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)))).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 1, 1), REQUEST_ID));

    xProductOutboundBean.getItemBasicDetailV2Response(Arrays.asList(PRODUCT_SKU), false);

    Mockito.verify(xProductFeign).getItemBasicDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void getItemBasicDetailsTestErrorTest() {
    Mockito.when(xProductFeign.getItemBasicDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)))).thenReturn(
        new GdnRestListResponse<>(null, null, false, new ArrayList<>(), new PageMetaData(0, 1, 1), REQUEST_ID));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getItemBasicDetailV2Response(Arrays.asList(PRODUCT_SKU), false);
      });
    } finally {
      Mockito.verify(xProductFeign).getItemBasicDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
          new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
    }
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void getItemBasicDetailsByItemSkusExceptionTest() {
    Mockito.when(xProductFeign.getItemBasicDetailsByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false,
            new SimpleListStringRequest(Collections.singletonList(DEFAULT_ITEM_CODE))))
        .thenReturn(new GdnRestListResponse<>());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getItemBasicDetailsByItemSkus(false,
            new SimpleListStringRequest(Collections.singletonList(DEFAULT_ITEM_CODE)));
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getItemBasicDetailsByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), false,
              new SimpleListStringRequest(Collections.singletonList(DEFAULT_ITEM_CODE)));
    }
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void getItemBasicDetailsByItemSkusTest() {
    Mockito.when(xProductFeign.getItemBasicDetailsByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false,
            new SimpleListStringRequest(Collections.singletonList(DEFAULT_ITEM_CODE))))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), null, null));
    xProductOutboundBean.getItemBasicDetailsByItemSkus(false,
        new SimpleListStringRequest(Collections.singletonList(DEFAULT_ITEM_CODE)));
    Mockito.verify(xProductFeign)
        .getItemBasicDetailsByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false,
            new SimpleListStringRequest(Collections.singletonList(DEFAULT_ITEM_CODE)));
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void getSecondaryCountsSuccessTest() {
    GdnRestSingleResponse<ProductCountResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    ProductCountResponse productCountResponse = new ProductCountResponse();
    productCountResponse.setOutOfStock(1L);
    productCountResponse.setActive(2L);
    response.setValue(productCountResponse);
    Mockito.when(xProductFeign
        .getSecondaryCounts(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), Constants.SECONDARY, DEFAULT_MERCHANT_CODE))
        .thenReturn(response);
    xProductOutboundBean.getSecondaryCounts(Constants.SECONDARY, DEFAULT_MERCHANT_CODE);
    Mockito.verify(xProductFeign)
        .getSecondaryCounts(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), Constants.SECONDARY, DEFAULT_MERCHANT_CODE);
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void getSecondaryCountsExceptionTest() {
    GdnRestSingleResponse<ProductCountResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    Mockito.when(xProductFeign
        .getSecondaryCounts(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), Constants.SECONDARY, DEFAULT_MERCHANT_CODE))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getSecondaryCounts(Constants.SECONDARY, DEFAULT_MERCHANT_CODE);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getSecondaryCounts(GdnMandatoryRequestParameterUtil.getStoreId(),
              GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
              GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), Constants.SECONDARY, DEFAULT_MERCHANT_CODE);
    }
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void getSecondaryCountsNullResponseTest() {
    Mockito.when(xProductFeign
        .getSecondaryCounts(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), Constants.SECONDARY, DEFAULT_MERCHANT_CODE))
        .thenReturn(new GdnRestSingleResponse<>());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getSecondaryCounts(Constants.SECONDARY, DEFAULT_MERCHANT_CODE);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getSecondaryCounts(GdnMandatoryRequestParameterUtil.getStoreId(),
              GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
              GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), Constants.SECONDARY, DEFAULT_MERCHANT_CODE);
    }
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void getSecondaryCountsNullResponseAndSuccessTrueTest() {
    GdnRestSingleResponse<ProductCountResponse> gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(true);
    Mockito.when(xProductFeign
        .getSecondaryCounts(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), Constants.SECONDARY, DEFAULT_MERCHANT_CODE))
        .thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getSecondaryCounts(Constants.SECONDARY, DEFAULT_MERCHANT_CODE);
      });
    } finally {
      Mockito.verify(xProductFeign)
          .getSecondaryCounts(GdnMandatoryRequestParameterUtil.getStoreId(),
              GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
              GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), Constants.SECONDARY, DEFAULT_MERCHANT_CODE);
    }
    Assertions.assertNotNull(itemPickupPointRequest);
  }

  @Test
  public void getL5CountByItemSkuTest() {
    Mockito.when(xProductFeign.getL5CountByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            DEFAULT_ITEM_SKU))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new SimpleLongResponse(1L), REQUEST_ID));
    SimpleLongResponse simpleLongResponse = xProductOutboundBean.getL5CountByItemSku(DEFAULT_ITEM_SKU);
    Mockito.verify(xProductFeign).getL5CountByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        DEFAULT_ITEM_SKU);
    Assertions.assertEquals(1L, simpleLongResponse.getValue().longValue());
  }

  @Test
  public void getL5CountByItemSkuErrorTest() {
    try {
      Mockito.when(xProductFeign.getL5CountByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
              GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
              GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
              DEFAULT_ITEM_SKU))
          .thenReturn(new GdnRestSingleResponse<>(null, null, false, new SimpleLongResponse(1L), REQUEST_ID));
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getL5CountByItemSku(DEFAULT_ITEM_SKU);
      });
    } finally {
      Mockito.verify(xProductFeign).getL5CountByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          DEFAULT_ITEM_SKU);
    }
  }

  @Test
  public void getL5CountByItemSkuNullValueTest() {
    try {
      Mockito.when(xProductFeign.getL5CountByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          DEFAULT_ITEM_SKU)).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getL5CountByItemSku(DEFAULT_ITEM_SKU);
      });
    } finally {
      Mockito.verify(xProductFeign).getL5CountByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          DEFAULT_ITEM_SKU);
    }
  }

  @Test
  public void reconcileProductVariantsTest() throws Exception {
    GdnRestListResponse<ItemPickupPointCodeResponse> response = new GdnRestListResponse<>();
    response.setContent(new ArrayList<>());
    response.setSuccess(Boolean.TRUE);
    com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest addDeleteVariantRetryRequest =
        new com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest();
    Mockito.when(xProductFeign.reconcileProductVariants(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU,
        addDeleteVariantRetryRequest)).thenReturn(response);
    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList =
        xProductOutboundBean.reconcileProductVariants(addDeleteVariantRetryRequest, PRODUCT_SKU);
    Mockito.verify(xProductFeign).reconcileProductVariants(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU,
        addDeleteVariantRetryRequest);
    Assertions.assertEquals(Collections.emptyList(), itemPickupPointCodeResponseList);
  }

  @Test
  public void reconcileProductVariantsExceptionTest() throws Exception {
    com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest addDeleteVariantRetryRequest =
        new com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest();
    Mockito.when(xProductFeign.reconcileProductVariants(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),PRODUCT_SKU,
        addDeleteVariantRetryRequest)).thenReturn(new GdnRestListResponse<>());
    try {
      xProductOutboundBean.reconcileProductVariants(addDeleteVariantRetryRequest,PRODUCT_SKU);
    } catch (ApplicationRuntimeException applicationRuntimeException) {
      Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationRuntimeException.getErrorCodes());
    } finally {
      Mockito.verify(xProductFeign).reconcileProductVariants(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),PRODUCT_SKU,
          addDeleteVariantRetryRequest);
    }
  }

  @Test
  public void getSharedProductBundleRecipeDetailsTest() {
    Mockito.when(xProductFeign.getSharedProductBundleRecipeDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        new SimpleSetStringRequest(new HashSet<>()))).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 0, 0), REQUEST_ID));
    xProductOutboundBean.getSharedProductBundleRecipeDetails(new HashSet<>());
    Mockito.verify(xProductFeign).getSharedProductBundleRecipeDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        new SimpleSetStringRequest(new HashSet<>()));

  }

  @Test
  public void getSharedProductBundleRecipeDetailsErrorTest() {
    Mockito.when(xProductFeign.getSharedProductBundleRecipeDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        new SimpleSetStringRequest(new HashSet<>()))).thenReturn(
        new GdnRestListResponse<>(null, null, false, new ArrayList<>(), new PageMetaData(0, 0, 0), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.getSharedProductBundleRecipeDetails(new HashSet<>());
      });
    } finally {
      Mockito.verify(xProductFeign).getSharedProductBundleRecipeDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          new SimpleSetStringRequest(new HashSet<>()));
    }
  }

  @Test
  public void fetchBasicDetailsByItemSkuAndPickupPointCodeListTest() {
    ItemPickupPointRequest itemPickupPointRequest1 =
      ItemPickupPointRequest.builder().itemSku(DEFAULT_ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
        .build();
    GdnRestListResponse<ItemPickupPointBasicResponse> response = new GdnRestListResponse<>();
    response.setContent(Collections.emptyList());
    response.setSuccess(Boolean.TRUE);
    Mockito.when(xProductFeign.fetchBasicDetailsByItemSkuAndPickupPointCodeList(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(), Collections.singletonList(itemPickupPointRequest1))).thenReturn(response);
    xProductOutboundBean.fetchBasicDetailsByItemSkuAndPickupPointCodeList(Collections.singletonList(itemPickupPointRequest1));
    Mockito.verify(xProductFeign).fetchBasicDetailsByItemSkuAndPickupPointCodeList(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(), Collections.singletonList(itemPickupPointRequest1));
  }

  @Test
  public void fetchBasicDetailsByItemSkuAndPickupPointCodeListExceptionTest() {
    ItemPickupPointRequest itemPickupPointRequest1 =
      ItemPickupPointRequest.builder().itemSku(DEFAULT_ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
        .build();
    GdnRestListResponse<ItemPickupPointBasicResponse> response = new GdnRestListResponse<>();
    response.setContent(Collections.emptyList());
    response.setSuccess(Boolean.FALSE);
    Mockito.when(xProductFeign.fetchBasicDetailsByItemSkuAndPickupPointCodeList(
      GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      Collections.singletonList(itemPickupPointRequest1))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.fetchBasicDetailsByItemSkuAndPickupPointCodeList(
            Collections.singletonList(itemPickupPointRequest1));
      });
    } finally {
      Mockito.verify(xProductFeign).fetchBasicDetailsByItemSkuAndPickupPointCodeList(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(itemPickupPointRequest1));
    }
  }

  @Test
  public void getProductSkuDetailResponseTest() {
    Mockito.when(
      xProductFeign.getProductSkuDetailResponse(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU)).thenReturn(
      new GdnRestSingleResponse<>(null, null, true, productCenterDetailResponse, REQUEST_ID));
    ProductCenterDetailResponse response =
      xProductOutboundBean.getProductSkuDetailResponse(PRODUCT_SKU);
    Mockito.verify(xProductFeign)
      .getProductSkuDetailResponse(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
    Assertions.assertEquals(PRODUCT_CODE, productCenterDetailResponse.getProductName());
  }

  @Test
  public void getProductSkuDetailResponseSuccessFalseTest() {
    Mockito.when(
      xProductFeign.getProductSkuDetailResponse(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU)).thenReturn(
      new GdnRestSingleResponse<>(null, null, false, productCenterDetailResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
            xProductOutboundBean.getProductSkuDetailResponse(PRODUCT_SKU);
      });
    } finally {
      Mockito.verify(xProductFeign)
        .getProductSkuDetailResponse(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
    }
  }

  @Test
  public void getProductSkuDetailResponseValueNullTest() {
    Mockito.when(
        xProductFeign.getProductSkuDetailResponse(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        ProductCenterDetailResponse response =
            xProductOutboundBean.getProductSkuDetailResponse(PRODUCT_SKU);
      });
    } finally {
      Mockito.verify(xProductFeign)
        .getProductSkuDetailResponse(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
    }
  }

  @Test
  public void migrateProductAndL5DetailByProductSkuTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    ProductAndL5MigrationRequest productAndL5MigrationRequest = new ProductAndL5MigrationRequest();
    productAndL5MigrationRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(xProductFeign.migrateProductAndL5DetailByProductSku(
        Constants.DEFAULT_STORE_ID, Constants.PBP,
        Constants.DEFAULT_CHANNEL_ID, Constants.PBP, Constants.DEFAULT_USERNAME, productAndL5MigrationRequest))
      .thenReturn(response);
    xProductOutboundBean.migrateProductAndL5DetailByProductSku(productAndL5MigrationRequest,
      Constants.DEFAULT_STORE_ID, Constants.DEFAULT_USERNAME);
    Mockito.verify(xProductFeign)
      .migrateProductAndL5DetailByProductSku(Constants.DEFAULT_STORE_ID, Constants.PBP,
        Constants.DEFAULT_CHANNEL_ID, Constants.PBP, Constants.DEFAULT_USERNAME, productAndL5MigrationRequest);
  }

  @Test
  public void migrateProductAndL5DetailByProductSkuErrorTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(false);
    ProductAndL5MigrationRequest productAndL5MigrationRequest = new ProductAndL5MigrationRequest();
    productAndL5MigrationRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(
      xProductFeign.migrateProductAndL5DetailByProductSku(Constants.DEFAULT_STORE_ID, Constants.PBP,
        Constants.DEFAULT_CHANNEL_ID, Constants.PBP, Constants.DEFAULT_USERNAME,
        productAndL5MigrationRequest)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.migrateProductAndL5DetailByProductSku(productAndL5MigrationRequest,
            Constants.DEFAULT_STORE_ID, Constants.DEFAULT_USERNAME);
      });
    } finally {
      Mockito.verify(xProductFeign)
        .migrateProductAndL5DetailByProductSku(Constants.DEFAULT_STORE_ID, Constants.PBP,
          Constants.DEFAULT_CHANNEL_ID, Constants.PBP, Constants.DEFAULT_USERNAME,
          productAndL5MigrationRequest);
    }
  }

  @Test
  public void fetchL5ResponsesByItemSkusTest() {
    List<String> itemSkus = Arrays.asList(DEFAULT_ITEM_SKU, ITEM_SKU_2);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest(itemSkus, true);

    GdnRestListResponse<ItemPickupPointL5Response> l5Responses = new GdnRestListResponse<>();
    ItemPickupPointL5Response response1 = new ItemPickupPointL5Response();
    response1.setItemSku(ITEM_SKU_2);
    response1.setPickUpPointCode(PICKUP_POINT_CODE);
    ItemPickupPointL5Response response2 = new ItemPickupPointL5Response();
    response2.setItemSku(DEFAULT_ITEM_SKU);
    response2.setPickUpPointCode(PICKUP_POINT_CODE);
    l5Responses.setContent(Arrays.asList(response1, response2));
    l5Responses.setSuccess(true);
    Mockito.when(xProductFeign.findByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID,
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), PAGE, SIZE, simpleListStringRequest))
      .thenReturn(l5Responses);
    GdnRestListResponse<ItemPickupPointL5Response> result =
      xProductOutboundBean.fetchL5ResponsesByItemSkus(itemSkus, PAGE, SIZE);
    Assertions.assertNotNull(result);
    Mockito.verify(xProductFeign).findByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
      Constants.DEFAULT_CHANNEL_ID,
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(), PAGE, SIZE, simpleListStringRequest);
  }

  @Test()
  public void fetchL5ResponsesByItemSkusResponseFailureTest() {
    List<String> itemSkus = Arrays.asList(DEFAULT_ITEM_SKU, ITEM_SKU_2);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest(itemSkus, true);
    GdnRestListResponse<ItemPickupPointL5Response> result ;
    GdnRestListResponse<ItemPickupPointL5Response> l5Responses = new GdnRestListResponse<>();
    ItemPickupPointL5Response response1 = new ItemPickupPointL5Response();
    response1.setItemSku(ITEM_SKU_2);
    response1.setPickUpPointCode(PICKUP_POINT_CODE);
    ItemPickupPointL5Response response2 = new ItemPickupPointL5Response();
    response2.setItemSku(DEFAULT_ITEM_SKU);
    response2.setPickUpPointCode(PICKUP_POINT_CODE);
    l5Responses.setContent(Arrays.asList(response1, response2));
    l5Responses.setSuccess(false);
    Mockito.when(xProductFeign.findByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID,
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),PAGE, SIZE, simpleListStringRequest))
      .thenReturn(l5Responses);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        xProductOutboundBean.fetchL5ResponsesByItemSkus(itemSkus, PAGE, SIZE);
      });
    } finally {
      Mockito.verify(xProductFeign).findByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID,
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), PAGE, SIZE, simpleListStringRequest);
    }
  }

  @Test
  public void fetchL5ResponsesByItemSkusResponseNullResponseTest() {
    List<String> itemSkus = Arrays.asList(DEFAULT_ITEM_SKU, ITEM_SKU_2);
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest(itemSkus, true);

    GdnRestListResponse<ItemPickupPointL5Response> l5Responses = new GdnRestListResponse<>();
    ItemPickupPointL5Response response1 = new ItemPickupPointL5Response();
    response1.setItemSku(ITEM_SKU_2);
    response1.setPickUpPointCode(PICKUP_POINT_CODE);
    ItemPickupPointL5Response response2 = new ItemPickupPointL5Response();
    response2.setItemSku(DEFAULT_ITEM_SKU);
    response2.setPickUpPointCode(PICKUP_POINT_CODE);
    l5Responses.setContent(null);
    l5Responses.setSuccess(true);
    Mockito.when(xProductFeign.findByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID,
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), PAGE, SIZE, simpleListStringRequest))
      .thenReturn(l5Responses);
    GdnRestListResponse<ItemPickupPointL5Response> result =
      xProductOutboundBean.fetchL5ResponsesByItemSkus(itemSkus, PAGE, SIZE);
    Assertions.assertTrue(Objects.nonNull(result));
    Mockito.verify(xProductFeign).findByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
      Constants.DEFAULT_CHANNEL_ID,
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(), PAGE, SIZE, simpleListStringRequest);
  }

  @Test
  public void updateProductMasterDataInfoTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    ProductBasicMasterFieldsRequest productBasicMasterFieldsRequest =
      new ProductBasicMasterFieldsRequest();
    productBasicMasterFieldsRequest.setProductSku(PRODUCT_SKU);

    Mockito.when(
      xProductFeign.updateProductMasterDataInfo(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), Constants.DEFAULT_USERNAME,
        productBasicMasterFieldsRequest)).thenReturn(response);

    xProductOutboundBean.updateProductMasterDataInfo(Constants.DEFAULT_USERNAME,
      productBasicMasterFieldsRequest);

    Mockito.verify(xProductFeign)
      .updateProductMasterDataInfo(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), Constants.DEFAULT_USERNAME,
        productBasicMasterFieldsRequest);
  }

  @Test
  public void updateProductMasterDataInfoErrorTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(false);
    ProductBasicMasterFieldsRequest productBasicMasterFieldsRequest =
      new ProductBasicMasterFieldsRequest();
    productBasicMasterFieldsRequest.setProductSku(PRODUCT_SKU);

    Mockito.when(
      xProductFeign.updateProductMasterDataInfo(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), Constants.DEFAULT_USERNAME,
        productBasicMasterFieldsRequest)).thenReturn(response);

    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
        xProductOutboundBean.updateProductMasterDataInfo(Constants.DEFAULT_USERNAME,
          productBasicMasterFieldsRequest);
      });
    } finally {
      Mockito.verify(xProductFeign)
        .updateProductMasterDataInfo(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), Constants.DEFAULT_USERNAME,
          productBasicMasterFieldsRequest);
    }
  }

  @Test
  public void getCncAtL5ByProductSku_FailureTest_isSuccessFalse() {
    Mockito.when(xProductFeign.getCncAtL5ByProductSku(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        PRODUCT_SKU))
      .thenReturn(new GdnRestSimpleResponse<>(null, null, false, "error", null));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      xProductOutboundBean.getCncAtL5ByProductSku(PRODUCT_SKU);
    });
    Mockito.verify(xProductFeign).getCncAtL5ByProductSku(
      GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(),
      PRODUCT_SKU);
  }

  @Test
  public void getCncAtL5ByProductSku_SuccessTest() {
    com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse simpleBooleanResponse =
      new com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse();
    GdnRestSimpleResponse<com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse>
      response = new GdnRestSimpleResponse<>();
    response.setValue(simpleBooleanResponse);
    response.setSuccess(true);
    Mockito.when(xProductFeign.getCncAtL5ByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU)).thenReturn(response);
    SimpleBooleanResponse result = xProductOutboundBean.getCncAtL5ByProductSku(PRODUCT_SKU);
    Assertions.assertNotNull(result);
    Mockito.verify(xProductFeign)
      .getCncAtL5ByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
  }

  @Test
  public void getCncAtL5ByProductSku_FailedTest() {
    com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse simpleBooleanResponse =
      new com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse();
    GdnRestSimpleResponse<com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse>
      response = new GdnRestSimpleResponse<>();
    response.setValue(null);
    response.setSuccess(true);
    Mockito.when(xProductFeign.getCncAtL5ByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU)).thenReturn(response);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      xProductOutboundBean.getCncAtL5ByProductSku(PRODUCT_SKU);
    });
    Mockito.verify(xProductFeign)
      .getCncAtL5ByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_SKU);
  }

  @Test
  public void testUpdateCogsValueSuccess() throws Exception {
    CogsUpdateListRequest request = new CogsUpdateListRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);

    Mockito.when(xProductFeign.updateCogsValue(eq(PRODUCT_SKU), eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(GdnMandatoryRequestParameterUtil.getUsername()),
        eq(request))).thenReturn(response);

    xProductOutboundBean.updateCogsValue(PRODUCT_SKU, request);

    Mockito.verify(xProductFeign).updateCogsValue(eq(PRODUCT_SKU), eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(GdnMandatoryRequestParameterUtil.getUsername()),
        eq(request));
  }

  @Test
  public void testUpdateCogsValueFailure() {
    CogsUpdateListRequest request = new CogsUpdateListRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(false);
    response.setErrorMessage("Error message");

    Mockito.when(xProductFeign.updateCogsValue(eq(PRODUCT_SKU), eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(GdnMandatoryRequestParameterUtil.getUsername()),
        eq(request))).thenReturn(response);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      xProductOutboundBean.updateCogsValue(PRODUCT_SKU, request);
    });

    Mockito.verify(xProductFeign).updateCogsValue(eq(PRODUCT_SKU), eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(GdnMandatoryRequestParameterUtil.getUsername()),
        eq(request));
  }

  @Test
  public void testGetCogsDataSuccess() throws Exception {
    CogsResponse cogsResponse = new CogsResponse();
    List<CogsResponse> cogsList = Arrays.asList(cogsResponse);
    GdnRestListResponse<CogsResponse> response = new GdnRestListResponse<>();
    response.setSuccess(true);
    response.setContent(cogsList);

    Mockito.when(xProductFeign.getCogsData(eq(PRODUCT_SKU), eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(0),
        eq(10))).thenReturn(response);

    List<CogsResponse> result = xProductOutboundBean.getCogsData(PRODUCT_SKU, 0, 10);

    Assertions.assertEquals(cogsList, result);
    Mockito.verify(xProductFeign).getCogsData(eq(PRODUCT_SKU), eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(0),
        eq(10));
  }

  @Test
  public void testGetCogsDataFailure() {
    GdnRestListResponse<CogsResponse> response = new GdnRestListResponse<>();
    response.setSuccess(false);
    response.setErrorMessage("Error message");

    Mockito.when(xProductFeign.getCogsData(eq(PRODUCT_SKU), eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(0),
        eq(10))).thenReturn(response);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      xProductOutboundBean.getCogsData(PRODUCT_SKU, 0, 10);
    });

    Mockito.verify(xProductFeign).getCogsData(eq(PRODUCT_SKU), eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(0),
        eq(10));
  }

  @Test
  public void updateProductWithConvertPreOrderDateToJKTEnabledTest() throws Exception {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);

    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    productRequest.setPreOrder(preOrderDTO);

    ArgumentCaptor<ProductRequest> captor = ArgumentCaptor.forClass(ProductRequest.class);

    Mockito.when(this.xProductFeign.updateProduct(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID),
            eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(GdnMandatoryRequestParameterUtil.getUsername()),
            eq(false), any(ProductRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productResponse, DEFAULT_REQUEST_ID));

    ProductResponse response = xProductOutboundBean.updateProduct(false, productRequest);

    Mockito.verify(this.xProductFeign)
        .updateProduct(eq(GdnMandatoryRequestParameterUtil.getStoreId()), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            eq(GdnMandatoryRequestParameterUtil.getUsername()), eq(false), captor.capture());

    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, response.getProductCode());
    Assertions.assertNotNull(captor.getValue().getPreOrder());
    Assertions.assertTrue((Boolean) ReflectionTestUtils.getField(productRequest.getPreOrder(), "convertToJKT"));
  }

  @Test
  public void updateProductWithConvertPreOrderDateToJKTDisabledTest() throws Exception {
    org.springframework.test.util.ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", false);
    com.gdn.x.product.rest.web.model.dto.PreOrderDTO preOrderDTO = new com.gdn.x.product.rest.web.model.dto.PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    productRequest.setPreOrder(preOrderDTO);
    Mockito.when(this.xProductFeign
        .updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, productRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productResponse, DEFAULT_REQUEST_ID));
    ProductResponse response =
        xProductOutboundBean.updateProduct(false, productRequest);
    Mockito.verify(this.xProductFeign)
        .updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, productRequest);
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, response.getProductCode());
  }

  @Test
  public void updateProductWithConvertPreOrderDateToJKTEnabledButPreOrderNullTest() throws Exception {
    org.springframework.test.util.ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    productRequest.setPreOrder(null);
    Mockito.when(this.xProductFeign
        .updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, productRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productResponse, DEFAULT_REQUEST_ID));
    ProductResponse response =
        xProductOutboundBean.updateProduct(false, productRequest);
    Mockito.verify(this.xProductFeign)
        .updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, productRequest);
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, response.getProductCode());
  }

  @Test
  public void updateProductWithConvertPreOrderDateToJKTEnabledButEditRequestNullTest() throws Exception {
    org.springframework.test.util.ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    Mockito.when(
            this.xProductFeign.updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                GdnMandatoryRequestParameterUtil.getUsername(), false, null))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productResponse, DEFAULT_REQUEST_ID));
    xProductOutboundBean.updateProduct(false, null);
    Mockito.verify(this.xProductFeign)
        .updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, null);
  }

  @Test
  public void updateEditedProductWithConvertPreOrderDateToJKTDisabledTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", false);
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    ProductRequest productRequestForEdit = new ProductRequest();
    productRequestForEdit.setPreOrder(preOrderDTO);
    productEditRequest.setProductRequest(productRequestForEdit);
    Mockito.when(xProductFeign
        .updateEditedProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, productEditRequest))
        .thenReturn(new GdnBaseRestResponse(true));
    xProductOutboundBean.updateEditedProduct(productEditRequest, false);
    Mockito.verify(xProductFeign)
        .updateEditedProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, productEditRequest);
  }

  @Test
  public void updateEditedProductWithConvertPreOrderDateToJKTEnabledButProductEditRequestNullTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    try {
      xProductOutboundBean.updateEditedProduct(null, false);
    } catch (Exception e) {
      Assertions.assertNotNull(e);
    } finally {
      Mockito.verify(xProductFeign)
          .updateEditedProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), false, null);
    }
  }

  @Test
  public void updateEditedProductWithConvertPreOrderDateToJKTEnabledButProductRequestNullTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    ProductEditRequest productEditRequestForTest = new ProductEditRequest();
    productEditRequestForTest.setProductRequest(null);
    Mockito.when(xProductFeign
        .updateEditedProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, productEditRequestForTest))
        .thenReturn(new GdnBaseRestResponse(true));
    xProductOutboundBean.updateEditedProduct(productEditRequestForTest, false);
    Mockito.verify(xProductFeign)
        .updateEditedProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, productEditRequestForTest);
  }

  @Test
  public void updateEditedProductWithConvertPreOrderDateToJKTEnabledButPreOrderNullTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    ProductRequest productRequestForEdit = new ProductRequest();
    productRequestForEdit.setPreOrder(null);
    productEditRequest.setProductRequest(productRequestForEdit);
    Mockito.when(xProductFeign
        .updateEditedProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, productEditRequest))
        .thenReturn(new GdnBaseRestResponse(true));
    xProductOutboundBean.updateEditedProduct(productEditRequest, false);
    Mockito.verify(xProductFeign)
        .updateEditedProduct(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, productEditRequest);
  }

  @Test
  public void updateEditedProductAndItemPickupPointWithConvertPreOrderDateToJKTDisabledTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", false);
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    ProductEditRequest productEditRequestForUpdate = new ProductEditRequest();
    ProductRequest productRequestForUpdate = new ProductRequest();
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    productRequestForUpdate.setPreOrder(preOrderDTO);
    productEditRequestForUpdate.setProductRequest(productRequestForUpdate);
    productDetailPageEditRequest.setProductEditRequest(productEditRequestForUpdate);
    Mockito.when(xProductFeign
        .updateEditedProductAndItemPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, productDetailPageEditRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new CombinedEditItemResponse(), DEFAULT_REQUEST_ID));
    xProductOutboundBean.updateEditedProductAndItemPickupPoint(PRODUCT_SKU, false, productDetailPageEditRequest);
    Mockito.verify(xProductFeign)
        .updateEditedProductAndItemPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, productDetailPageEditRequest);
  }

  @Test
  public void updateEditedProductAndItemPickupPointWithConvertPreOrderDateToJKTEnabledButProductDetailPageEditRequestNullTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    ProductDetailPageEditRequest nullProductDetailPageEditRequest = null;
    try {
      xProductOutboundBean.updateEditedProductAndItemPickupPoint(PRODUCT_SKU, false, nullProductDetailPageEditRequest);
    } catch (Exception e) {
      Assertions.assertNotNull(e);
    }
  }

  @Test
  public void updateEditedProductAndItemPickupPointWithConvertPreOrderDateToJKTEnabledButProductEditRequestNullTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    productDetailPageEditRequest.setProductEditRequest(null);
    Mockito.when(xProductFeign
        .updateEditedProductAndItemPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, productDetailPageEditRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new CombinedEditItemResponse(), DEFAULT_REQUEST_ID));
    xProductOutboundBean.updateEditedProductAndItemPickupPoint(PRODUCT_SKU, false, productDetailPageEditRequest);
    Mockito.verify(xProductFeign)
        .updateEditedProductAndItemPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, productDetailPageEditRequest);
  }

  @Test
  public void updateEditedProductAndItemPickupPointWithConvertPreOrderDateToJKTEnabledButProductRequestNullTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    ProductEditRequest productEditRequestForUpdate = new ProductEditRequest();
    productEditRequestForUpdate.setProductRequest(null);
    productDetailPageEditRequest.setProductEditRequest(productEditRequestForUpdate);
    Mockito.when(xProductFeign
        .updateEditedProductAndItemPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, productDetailPageEditRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new CombinedEditItemResponse(), DEFAULT_REQUEST_ID));
    xProductOutboundBean.updateEditedProductAndItemPickupPoint(PRODUCT_SKU, false, productDetailPageEditRequest);
    Mockito.verify(xProductFeign)
        .updateEditedProductAndItemPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, productDetailPageEditRequest);
  }

  @Test
  public void updateEditedProductAndItemPickupPointWithConvertPreOrderDateToJKTEnabledButPreOrderNullTest() {
    ReflectionTestUtils.setField(xProductOutboundBean, "convertPreOrderDateToJKT", true);
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    ProductEditRequest productEditRequestForUpdate = new ProductEditRequest();
    ProductRequest productRequestForUpdate = new ProductRequest();
    productRequestForUpdate.setPreOrder(null);
    productEditRequestForUpdate.setProductRequest(productRequestForUpdate);
    productDetailPageEditRequest.setProductEditRequest(productEditRequestForUpdate);
    Mockito.when(xProductFeign
        .updateEditedProductAndItemPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, productDetailPageEditRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new CombinedEditItemResponse(), DEFAULT_REQUEST_ID));
    xProductOutboundBean.updateEditedProductAndItemPickupPoint(PRODUCT_SKU, false, productDetailPageEditRequest);
    Mockito.verify(xProductFeign)
        .updateEditedProductAndItemPickupPoint(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, productDetailPageEditRequest);
  }
}
