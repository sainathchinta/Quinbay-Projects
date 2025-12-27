package com.gdn.mta.product.repository;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.xProduct.feign.PromoEligibilityRequest;
import com.gdn.partners.pbp.outbound.xProduct.feign.PromoEligibilityResponse;
import com.gdn.partners.pbp.outbound.xProduct.feign.XProductFeign;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.redis.core.BoundValueOperations;
import org.springframework.data.redis.core.RedisTemplate;

import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ProductLevel3RepositoryBeanTest {

  private static final String DEFAULT_GDN_SKU = "BLT-00001-00001-00001";
  private static final String DEFAULT_GDN_SKU_2 = "BLT-00001-00001-00002";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLT-00001";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final PageRequest DEFAULT_PAGE_REQUEST = PageRequest.of(0 ,10);
  private static final SortOrder DEFAULT_SORT = new SortOrder("createdDate", "desc");
  private static final String DEFAULT_PICKUP_POINT_CODE = "PPC-00001";
  private static final String BRAND = "brand";
  private static final String BUSINESS_PARTNER_CODE = "MTA-0001";
  private static final String USER_NAME = "userName";
  private static final String REQUEST_ID = "requestId";
  private static final String ITEM_CODE = "itemCode";
  private static final String ORDER = "ASC";
  private static final String DEFAULT_USERNAME = "PBP-API";
  private static final String CATALOG_CODE = "Catalog_code";
  private static final int PAGE_NUMBER = 0;
  private static final int PAGE_SIZE = 10;
  private static final String PRODUCT_SKU = "BLT-00001-00001";
  private static final String PRODUCT_CODE = "MTA-10000001";
  private static final boolean DO_SUSPENSION = true;
  private ActiveProductRequest activeProductRequest;
  private SummaryFilterRequest summaryFilterRequest;

  @Mock
  private XProductFeign xProductFeign;

  @Mock
  private RedisTemplate<String, Object> redisTemplate;

  @Mock
  private BoundValueOperations<String, Object> boundValueOperations;

  @InjectMocks
  private ProductLevel3RepositoryBean productLevel3RepositoryBean;

  private String requestId;

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.xProductFeign);
  }

  public XProductFeign getProductClient() {
    return xProductFeign;
  }

  public ProductLevel3RepositoryBean getProductLevel3RepositoryBean() {
    return productLevel3RepositoryBean;
  }

  public String getRequestId() {
    return requestId;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    setRequestId(UUID.randomUUID().toString());

    ProductAndItemsResponse productLevel3 = new ProductAndItemsResponse();
    GdnRestSingleResponse<ProductAndItemsResponse> responseDetail =
        new GdnRestSingleResponse<ProductAndItemsResponse>(null, null, true, productLevel3, getRequestId());
    GdnRestSingleResponse<ProductAndItemsResponse> responseDetailError =
        new GdnRestSingleResponse<ProductAndItemsResponse>("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            null, getRequestId());
    GdnRestListResponse<ItemSummaryResponse> responseSummaryFilter =
        new GdnRestListResponse<ItemSummaryResponse>(new ArrayList<ItemSummaryResponse>(), new PageMetaData(0, 0, 0),
            getRequestId());
    GdnRestSingleResponse<AddProductAndItemsResponse> responseAdd =
        new GdnRestSingleResponse<AddProductAndItemsResponse>(new AddProductAndItemsResponse(), DEFAULT_REQUEST_ID);
    GdnRestSingleResponse<ItemSummaryResponse> responseSummary =
        new GdnRestSingleResponse<ItemSummaryResponse>(new ItemSummaryResponse(), DEFAULT_REQUEST_ID);
    GdnRestSingleResponse<ItemResponse> responseItem =
        new GdnRestSingleResponse<ItemResponse>(new ItemResponse(), DEFAULT_REQUEST_ID);
    GdnRestSingleResponse<SimpleBooleanResponse> responseBoolean =
        new GdnRestSingleResponse<SimpleBooleanResponse>(new SimpleBooleanResponse(), DEFAULT_REQUEST_ID);

    when(getProductClient().getProductAndSingleItemByItemSku(Mockito.anyString(),
        Mockito.anyString(), any(), any(), any(),
        Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(responseDetail);
    when(getProductClient()
        .getProductDetailAndSingleItemByItemSku(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),
            Mockito.eq(DEFAULT_GDN_SKU)))
        .thenReturn(responseDetail);
    when(getProductClient().getProductAndSingleItemByItemSku(Mockito.anyString(), any(), any(), Mockito.anyString(),Mockito.anyString(), Mockito.eq(DEFAULT_GDN_SKU_2),Mockito.anyBoolean(),Mockito.anyBoolean()))
        .thenReturn(responseDetailError);
    when(getProductClient()
        .getListOfItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
            Mockito.anyString(), Mockito.anyString(), any())).thenReturn(responseSummaryFilter);
    when(getProductClient().addProductAndItems(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.any())).thenReturn(responseAdd);
    when(getProductClient()
        .updateItemSummary(Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any())).thenReturn(responseSummary);
    when(getProductClient().getItemSummaryByItemSku(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),  Mockito.anyString()))
        .thenReturn(responseSummary);
    when(getProductClient().getItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString())).thenReturn(responseItem);
    when(getProductClient().isPickupPointCodeUsed(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString()))
        .thenReturn(responseBoolean);
    when(getProductClient().getProductAndItems(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(responseDetail);
    activeProductRequest = new ActiveProductRequest();
    activeProductRequest.setSortType(ORDER);
    summaryFilterRequest = new SummaryFilterRequest();
    summaryFilterRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);

  }

  public void setProductLevel3RepositoryBean(ProductLevel3RepositoryBean productLevel3RepositoryBean) {
    this.productLevel3RepositoryBean = productLevel3RepositoryBean;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  @Test
  public void testFindDetailByGdnSku() throws Exception {
    getProductLevel3RepositoryBean().findDetailByGdnSku(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient())
        .getProductAndSingleItemByItemSku(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_GDN_SKU),Mockito.anyBoolean(), Mockito.anyBoolean());
  }

  @Test
  public void testFindProductAndItemDetailByGdnSku() throws Exception {
    getProductLevel3RepositoryBean().findProductAndItemDetailByGdnSku(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient()).getProductDetailAndSingleItemByItemSku(any(), any(), any(),Mockito.anyString(), Mockito.anyString(),
            Mockito.eq(DEFAULT_GDN_SKU));
  }

  @Test
  public void testFindProductAndItemDetailByGdnSkuExceptionTest() throws Exception {
    GdnRestSingleResponse<ProductAndItemsResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    when(xProductFeign.getProductDetailAndSingleItemByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        DEFAULT_GDN_SKU)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().findProductAndItemDetailByGdnSku(DEFAULT_GDN_SKU);
      });
    } finally {
      Mockito.verify(getProductClient())
          .getProductDetailAndSingleItemByItemSku(any(), any(), any(), Mockito.anyString(),
              Mockito.anyString(), Mockito.eq(DEFAULT_GDN_SKU));
    }
  }

  @Test
  public void findSummaryByCategoryAndBrandFilterTest() throws Exception {
    CampaignItemSummaryRequest campaignItemSummaryRequest = new CampaignItemSummaryRequest();
    PageRequest pageRequest = PageRequest.of(0, 10);
    SortOrder sortOrder = new SortOrder();
    GdnRestListResponse<ItemSummaryResponse> response = new GdnRestListResponse<>();
    when(
        xProductFeign.getListOfItemSummaryByCategoryAndBrandFilter(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageRequest.getPageNumber(), pageRequest.getPageSize(), sortOrder.getOrderBy(), sortOrder.getSortBy(),
            campaignItemSummaryRequest)).thenReturn(response);
    getProductLevel3RepositoryBean().findSummaryByCategoryAndBrandFilter(campaignItemSummaryRequest, pageRequest,
        sortOrder);

    Mockito.verify(xProductFeign)
        .getListOfItemSummaryByCategoryAndBrandFilter(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageRequest.getPageNumber(), pageRequest.getPageSize(), sortOrder.getOrderBy(), sortOrder.getSortBy(),
            campaignItemSummaryRequest);
  }

  @Test
  public void getItemNameByItemSkuTest() throws Exception {
    List<String> itemSkus = Arrays.asList(DEFAULT_GDN_SKU, DEFAULT_GDN_SKU_2);
    when(getProductClient().getItemNameByItemSkus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyBoolean(),
        any(
            SimpleListStringRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(new SimpleMapStringResponse(), REQUEST_ID));
    getProductLevel3RepositoryBean().getItemNameByItemSku(itemSkus);
    Mockito.verify(getProductClient())
        .getItemNameByItemSkus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyBoolean(),
            any(
            SimpleListStringRequest.class));
  }

  @Test
  public void getItemNameByItemSku_whenSuccessFalseTest() throws Exception {
    List<String> itemSkus = Arrays.asList(DEFAULT_GDN_SKU, DEFAULT_GDN_SKU_2);
    when(getProductClient().getItemNameByItemSkus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyBoolean(),
        any(
            SimpleListStringRequest.class)))
        .thenReturn(
            new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
                false, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().getItemNameByItemSku(itemSkus);
      });
    } finally {
      Mockito.verify(getProductClient())
          .getItemNameByItemSkus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyBoolean(),
              any(
              SimpleListStringRequest.class));
    }
  }

  @Test
  public void getProductNameByProductSkuTest() throws Exception {
    List<String> productSkus = Arrays.asList(DEFAULT_GDN_SKU, DEFAULT_GDN_SKU_2);
    when(getProductClient()
        .getProductNameByProductSku(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),
            any(
            SimpleListStringRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(new SimpleMapStringResponse(), REQUEST_ID));
    getProductLevel3RepositoryBean().getProductNameByProductSku(productSkus);
    Mockito.verify(getProductClient())
        .getProductNameByProductSku(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),
            any(
            SimpleListStringRequest.class));
  }

  @Test
  public void getProductNameByProductSku_whenSuccessFalseTest() throws Exception {
    List<String> productSkus = Arrays.asList(DEFAULT_GDN_SKU, DEFAULT_GDN_SKU_2);
    when(getProductClient()
        .getProductNameByProductSku(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),
            any(
            SimpleListStringRequest.class))).thenReturn(
        new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
            null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().getProductNameByProductSku(productSkus);
      });
    } finally {
      Mockito.verify(getProductClient())
          .getProductNameByProductSku(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),
              any(
              SimpleListStringRequest.class));
    }
  }

  @Test
  public void testFindDetailByGdnSkuWithMDCValue() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, getRequestId());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    getProductLevel3RepositoryBean().findDetailByGdnSku(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient())
        .getProductAndSingleItemByItemSku(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), any(),
            Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
  }

  @Test
  public void testFindDetailByGdnSkuWithError() throws Exception {
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().findDetailByGdnSku(DEFAULT_GDN_SKU_2);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(getProductClient())
        .getProductAndSingleItemByItemSku(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), any(),
            Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean());
  }

  @Test
  public void updateItemPriceTest() throws Exception {
    PriceRequest priceRequest = new PriceRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    when(getProductClient()
        .updateItemPrice(any(), any(), any(), any(), any(), any(), any())).thenReturn(response);
    getProductLevel3RepositoryBean().updateItemPrice(priceRequest, DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient())
        .updateItemPrice(any(), any(), any(), any(), any(), any(), any());
  }


  @Test
  public void getItemTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    getProductLevel3RepositoryBean().getItem(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient()).getItem(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void getItemTestException() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    GdnRestSingleResponse<ItemResponse> responseExceptionItem =
        new GdnRestSingleResponse<>("errorMessage", "errorCode", false, new ItemResponse(), DEFAULT_REQUEST_ID);
    when(xProductFeign.getItem(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(responseExceptionItem);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().getItem(DEFAULT_GDN_SKU);
      });
    } finally {
      Mockito.verify(xProductFeign).getItem(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    }
  }

  @Test
  public void getProductTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    getProductLevel3RepositoryBean().getProduct(DEFAULT_GDN_SKU);
    Mockito.verify(xProductFeign)
        .getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void getProductTestException() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    GdnRestSingleResponse<ProductAndItemsResponse> responseExceptionProduct =
        new GdnRestSingleResponse<>("errorMessage", "errorCode", false, new ProductAndItemsResponse(),
            DEFAULT_REQUEST_ID);
    when(xProductFeign.getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.anyString(), Mockito.anyBoolean()))
        .thenReturn(responseExceptionProduct);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().getProduct(DEFAULT_GDN_SKU);
      });
    } finally {
      Mockito.verify(xProductFeign).getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
          Mockito.anyString(), Mockito.anyBoolean());
    }
  }


  @Test
  public void updateItemPriceThrowExceptionTest() throws Exception {
    PriceRequest priceRequest = new PriceRequest();
    GdnBaseRestResponse response =
        new GdnBaseRestResponse(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), ErrorCategory.COMMUNICATION_FAILURE.getCode(), false, getRequestId());
    when(getProductClient()
        .updateItemPrice(any(), any(), any(), any(), any(), any(), any())).thenReturn(response);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        getProductLevel3RepositoryBean().updateItemPrice(priceRequest, DEFAULT_GDN_SKU);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(getProductClient())
        .updateItemPrice(any(), any(), any(), any(), any(), any(), any());
  }

  @Test
  public void updateItemViewConfigTest() throws Exception {
    ItemViewConfigRequest itemViewConfigRequest = new ItemViewConfigRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    when(getProductClient().updateItemViewConfig(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.eq(itemViewConfigRequest))).thenReturn(response);
    getProductLevel3RepositoryBean().updateItemViewConfig(itemViewConfigRequest, DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient())
        .updateItemViewConfig(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(itemViewConfigRequest));
  }

  @Test
  public void updateItemViewConfigThrowExceptionTest() throws Exception {
    ItemViewConfigRequest itemViewConfigRequest = new ItemViewConfigRequest();
    GdnBaseRestResponse response =
        new GdnBaseRestResponse(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), ErrorCategory.COMMUNICATION_FAILURE.getCode(), false, getRequestId());
    when(getProductClient().updateItemViewConfig(any(), any(), any(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.eq(itemViewConfigRequest))).thenReturn(response);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        getProductLevel3RepositoryBean().updateItemViewConfig(itemViewConfigRequest, DEFAULT_GDN_SKU);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(getProductClient())
        .updateItemViewConfig(any(), any(), any(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.eq(itemViewConfigRequest));
  }

  @Test
  public void testFindSummaryByFilter() throws Exception {
    getProductLevel3RepositoryBean()
        .findSummaryByFilter(DEFAULT_BUSINESS_PARTNER_CODE, null, null, null, null, null, null, null,
            DEFAULT_PAGE_REQUEST, null, DEFAULT_SORT);
    Mockito.verify(getProductClient())
        .getListOfItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyInt(),Mockito.anyInt(),
            Mockito.anyString(), Mockito.anyString(), any());
  }

  @Test
  public void testFindSummaryByFilter_sort_null() throws Exception {
    when(xProductFeign.getListOfItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
        any(),  any(), any(ItemSummaryRequest.class))).thenReturn(new GdnRestListResponse<>(null, null, true, List.of(new ItemSummaryResponse()), new PageMetaData(), null));
    getProductLevel3RepositoryBean()
        .findSummaryByFilter(DEFAULT_BUSINESS_PARTNER_CODE, null, null, null, null, null, null, null,
            DEFAULT_PAGE_REQUEST, null, null);
    Mockito.verify(getProductClient())
        .getListOfItemSummaryByFilter(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
            any(),  any(), any(ItemSummaryRequest.class));
  }


  @Test
  public void testFindSummaryByFilterWithError() throws Exception {
    // custom condition
    GdnRestListResponse<ItemSummaryResponse> responseSummaryFilterError =
        new GdnRestListResponse<ItemSummaryResponse>("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            DEFAULT_REQUEST_ID);
    when(getProductClient()
        .getListOfItemSummaryByFilter(any(), any(), any(), any(), any(),Mockito.anyInt(),
            Mockito.anyInt(), any(), any(), any())).thenReturn(responseSummaryFilterError);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    try {
    } catch (Exception e) {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean()
            .findSummaryByFilter(DEFAULT_BUSINESS_PARTNER_CODE, null, null, null, null, null, null, null,
                DEFAULT_PAGE_REQUEST, null, DEFAULT_SORT);
      });
      MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        Mockito.verify(getProductClient())
            .getListOfItemSummaryByFilter(any(), any(), any(), any(), any(),Mockito.anyInt(),
                Mockito.anyInt(), any(), any(), any());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void testSynchronizeProduct() throws Exception {
    ProductAndItemsResponse productLevel3 = new ProductAndItemsResponse();
    GdnRestSingleResponse<ProductAndItemsResponse> responseDetail =
        new GdnRestSingleResponse<ProductAndItemsResponse>(null, null, true, productLevel3, getRequestId());
    when(
        getProductClient().synchronizeProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_GDN_SKU))).thenReturn(responseDetail);
    getProductLevel3RepositoryBean().synchronizeProduct(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient()).synchronizeProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_GDN_SKU));
  }

  @Test
  public void testSynchronizeProductWithValue() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, getRequestId());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    ProductAndItemsResponse productLevel3 = new ProductAndItemsResponse();
    GdnRestSingleResponse<ProductAndItemsResponse> responseDetail =
        new GdnRestSingleResponse<ProductAndItemsResponse>(null, null, true, productLevel3, getRequestId());
    when(
        getProductClient().synchronizeProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_GDN_SKU))).thenReturn(responseDetail);
    getProductLevel3RepositoryBean().synchronizeProduct(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient()).synchronizeProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_GDN_SKU));
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
  }

  @Test
  public void testSynchronizeProductWithError() throws Exception {
    GdnRestSingleResponse<ProductAndItemsResponse> responseDetailError =
        new GdnRestSingleResponse<ProductAndItemsResponse>("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            null, getRequestId());
    when(
        getProductClient().synchronizeProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_GDN_SKU_2))).thenReturn(responseDetailError);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().synchronizeProduct(DEFAULT_GDN_SKU_2);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(getProductClient()).synchronizeProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_GDN_SKU_2));
  }

  @Test
  public void testUnsynchronizeProduct() throws Exception {
    ProductAndItemsResponse productLevel3 = new ProductAndItemsResponse();
    GdnRestSingleResponse<ProductAndItemsResponse> responseDetail =
        new GdnRestSingleResponse<ProductAndItemsResponse>(null, null, true, productLevel3, getRequestId());
    when(getProductClient()
        .unsynchronizeProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean())).thenReturn(responseDetail);
    getProductLevel3RepositoryBean().unsynchronizeProduct(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient())
        .unsynchronizeProduct(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
  }

  @Test
  public void testUnsynchronizeProductWithValue() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, getRequestId());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    ProductAndItemsResponse productLevel3 = new ProductAndItemsResponse();
    GdnRestSingleResponse<ProductAndItemsResponse> responseDetail =
        new GdnRestSingleResponse<ProductAndItemsResponse>(null, null, true, productLevel3, getRequestId());
    when(getProductClient()
        .unsynchronizeProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean())).thenReturn(responseDetail);
    getProductLevel3RepositoryBean().unsynchronizeProduct(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient())
        .unsynchronizeProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_GDN_SKU), Mockito.anyBoolean());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
  }

  @Test
  public void testUnsynchronizeProductWithError() throws Exception {
    GdnRestSingleResponse<ProductAndItemsResponse> responseDetailError =
        new GdnRestSingleResponse<ProductAndItemsResponse>("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            null, getRequestId());
    when(getProductClient()
        .unsynchronizeProduct(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.eq(DEFAULT_GDN_SKU_2), Mockito.anyBoolean())).thenReturn(responseDetailError);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().unsynchronizeProduct(DEFAULT_GDN_SKU_2);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(getProductClient())
        .unsynchronizeProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_GDN_SKU_2), Mockito.anyBoolean());
  }


  @Test
  public void updateSummaryTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    getProductLevel3RepositoryBean().updateSummary(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU, new UpdateItemSummaryRequest());
    Mockito.verify(getProductClient())
        .updateItemSummary(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString(), Mockito.anyString(), Mockito.any());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
  }

  @Test
  public void updateSummaryTestWithError() throws Exception {
    GdnRestSingleResponse<ItemSummaryResponse> responseSummaryError =
        new GdnRestSingleResponse<ItemSummaryResponse>("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false, null,
            requestId);
    when(getProductClient()
        .updateItemSummary(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString(), Mockito.any())).thenReturn(responseSummaryError);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().updateSummary(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU, new UpdateItemSummaryRequest());
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(getProductClient())
        .updateItemSummary(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString(),
            Mockito.any());
  }

  @Test
  public void findSummaryByGdnSkuTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    getProductLevel3RepositoryBean().findSummaryByGdnSku(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient()).getItemSummaryByItemSku(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
  }

  @Test
  public void findSummaryByGdnSkuTestWithError() throws Exception {
    GdnRestSingleResponse<ItemSummaryResponse> responseSummaryError =
        new GdnRestSingleResponse<ItemSummaryResponse>("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false, null,
            DEFAULT_REQUEST_ID);
    when(getProductClient().getItemSummaryByItemSku(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString()))
        .thenReturn(responseSummaryError);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().findSummaryByGdnSku(DEFAULT_GDN_SKU);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(getProductClient()).getItemSummaryByItemSku(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void findArchivedSummaryByGdnSkuTest() throws Exception {
    GdnRestSingleResponse<ItemSummaryResponse> responseSummary =
        new GdnRestSingleResponse<ItemSummaryResponse>(null, null, true, null, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    when(getProductClient().getArchivedItemSummaryByItemSku(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(responseSummary);
    getProductLevel3RepositoryBean().findSummaryByArchivedGdnSku(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient()).getArchivedItemSummaryByItemSku(Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString(), Mockito.anyString());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
  }

  @Test
  public void findSummaryByArchivedGdnSkuTestWithError() throws Exception {
    GdnRestSingleResponse<ItemSummaryResponse> responseSummaryError =
        new GdnRestSingleResponse<ItemSummaryResponse>("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false, null,
            DEFAULT_REQUEST_ID);
    when(getProductClient().getArchivedItemSummaryByItemSku(Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString(), Mockito.anyString()))
        .thenReturn(responseSummaryError);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().findSummaryByArchivedGdnSku(DEFAULT_GDN_SKU);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(getProductClient())
        .getArchivedItemSummaryByItemSku(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString());
  }

  @Test
  public void checkPickupPointCodeUsedTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    getProductLevel3RepositoryBean().checkPickupPointCodeUsed(DEFAULT_PICKUP_POINT_CODE);
    Mockito.verify(getProductClient()).isPickupPointCodeUsed(Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString(), Mockito.anyString());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
  }

  @Test
  public void checkPickupPointCodeUsedTestWithError() throws Exception {
    GdnRestSingleResponse<SimpleBooleanResponse> responseBooleanError =
        new GdnRestSingleResponse<SimpleBooleanResponse>("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            null, DEFAULT_REQUEST_ID);
    when(getProductClient().isPickupPointCodeUsed(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString()))
        .thenReturn(responseBooleanError);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().checkPickupPointCodeUsed(DEFAULT_PICKUP_POINT_CODE);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(getProductClient()).isPickupPointCodeUsed(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString());
  }

  @Test
  public void updateItemOff2OnActivateTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    when(getProductClient().activateOff2OnChannelActive(Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString(), Mockito.eq(DEFAULT_GDN_SKU)))
        .thenReturn(response);
    getProductLevel3RepositoryBean().updateItemOff2OnActivate(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient()).activateOff2OnChannelActive(Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.anyString(), Mockito.eq(DEFAULT_GDN_SKU));
  }

  @Test
  public void updateItemOff2OnActivateTestWithError() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(false);
    when(getProductClient().activateOff2OnChannelActive(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.eq(DEFAULT_GDN_SKU)))
        .thenReturn(response);
    try {
      getProductLevel3RepositoryBean().updateItemOff2OnActivate(DEFAULT_GDN_SKU);
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(getProductClient()).activateOff2OnChannelActive(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(DEFAULT_GDN_SKU));
      }
    }
  }

  @Test
  public void updateItemOff2OnDeactivateTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    when(getProductClient().deactivateOff2OnChannelActive(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.eq(DEFAULT_GDN_SKU)))
        .thenReturn(response);
    getProductLevel3RepositoryBean().updateItemOff2OnDeactivate(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient()).deactivateOff2OnChannelActive(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),  Mockito.eq(DEFAULT_GDN_SKU));
  }

  @Test
  public void updateItemOff2OnDeactivateTestWithError() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(false);
    when(getProductClient().deactivateOff2OnChannelActive(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(DEFAULT_GDN_SKU)))
        .thenReturn(response);
    try {
      getProductLevel3RepositoryBean().updateItemOff2OnDeactivate(DEFAULT_GDN_SKU);
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(getProductClient())
            .deactivateOff2OnChannelActive(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(DEFAULT_GDN_SKU));
      }
    }
  }

  @Test
  public void testFindDetailByProductSku() throws Exception {
    getProductLevel3RepositoryBean().findDetailByProductSku(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient()).getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void testFindDetailByProductSku_2() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    getProductLevel3RepositoryBean().findDetailByProductSku(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient()).getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyString(), Mockito.anyBoolean());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
  }

  @Test
  public void testFindDetailByProductSku_Failed() throws Exception {
    when(getProductClient().getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.anyString(), Mockito.anyBoolean()))
        .thenReturn(new GdnRestSingleResponse<ProductAndItemsResponse>(null, null, false, null, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class, () -> {
        getProductLevel3RepositoryBean().findDetailByProductSku(DEFAULT_GDN_SKU);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(getProductClient())
        .getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void findDetailByProductSkuForSuspensionTest() throws Exception {
    when(getProductClient()
        .getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.anyString(), Mockito.anyBoolean())).thenReturn(
        new GdnRestSingleResponse<ProductAndItemsResponse>(null, null, true, new ProductAndItemsResponse(),
            DEFAULT_REQUEST_ID));
    getProductLevel3RepositoryBean().findDetailByProductSkuForSuspension(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient())
        .getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void findDetailByProductSkuForSuspensionTest2() throws Exception {
    when(getProductClient()
        .getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.anyString(), Mockito.anyBoolean())).thenReturn(
        new GdnRestSingleResponse<ProductAndItemsResponse>(null, null, true, new ProductAndItemsResponse(),
            DEFAULT_REQUEST_ID));
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    getProductLevel3RepositoryBean().findDetailByProductSkuForSuspension(DEFAULT_GDN_SKU);
    Mockito.verify(getProductClient())
        .getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.anyString(), Mockito.anyBoolean());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
  }

  @Test
  public void findDetailByProductSkuForSuspension_exceptionTest() throws Exception {
    when(getProductClient()
        .getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.anyString(), Mockito.anyBoolean())).thenReturn(new GdnRestSingleResponse<ProductAndItemsResponse>(null, null, false, null, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class, () -> {
        getProductLevel3RepositoryBean().findDetailByProductSkuForSuspension(DEFAULT_GDN_SKU);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(getProductClient())
        .getProductAndItems(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void test_toggleArchiveItem() throws Exception {
    when(getProductClient()
        .toggleArchiveItem(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean()))
        .thenReturn(new GdnBaseRestResponse(true));
    getProductLevel3RepositoryBean().toggleArchiveItem("TOA-14961-12232-00001", true);
    Mockito.verify(getProductClient())
        .toggleArchiveItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void test_toggleArchiveItem_errorCase() throws Exception {
    when(getProductClient()
        .toggleArchiveItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyBoolean()))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      getProductLevel3RepositoryBean().toggleArchiveItem("TOA-14961-12232-00001", true);
      Mockito.verify(getProductClient())
          .toggleArchiveItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyBoolean());
      assertTrue(false);
    } catch (ApplicationException e) {
      Mockito.verify(getProductClient())
          .toggleArchiveItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyBoolean());
    }
  }

  @Test
  public void toggleSuspensionProductTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, getRequestId());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    when(getProductClient().toggleSuspensionProduct(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(getRequestId()), Mockito.eq(DEFAULT_USERNAME),Mockito.eq( PRODUCT_SKU), Mockito.eq(DO_SUSPENSION)))
        .thenReturn(new GdnBaseRestResponse(true));
    getProductLevel3RepositoryBean().toggleSuspensionProduct(PRODUCT_SKU, DO_SUSPENSION);
    Mockito.verify(getProductClient())
        .toggleSuspensionProduct(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(getRequestId()), Mockito.eq(DEFAULT_USERNAME),Mockito.eq( PRODUCT_SKU), Mockito.eq(DO_SUSPENSION));
  }

  @Test
  public void toggleSuspensionProductErrorTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, getRequestId());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    when(getProductClient().toggleSuspensionProduct(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(getRequestId()), Mockito.eq(DEFAULT_USERNAME), Mockito.eq(PRODUCT_SKU), Mockito.eq(DO_SUSPENSION)))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      getProductLevel3RepositoryBean().toggleSuspensionProduct(PRODUCT_SKU, DO_SUSPENSION);
    } catch (ApplicationException e) {
      Mockito.verify(getProductClient())
          .toggleSuspensionProduct(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(getRequestId()), Mockito.eq(DEFAULT_USERNAME), Mockito.eq(PRODUCT_SKU), Mockito.eq(DO_SUSPENSION));
    }
  }

  @Test
  public void getProductCountByBrandTest() throws Exception {
    SimpleLongResponse value = new SimpleLongResponse(1L);
    GdnRestSingleResponse<SimpleLongResponse> response =
        new GdnRestSingleResponse<>(null, null, true, value, DEFAULT_REQUEST_ID);
    when(getProductClient().getProductsCountByBrand(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString()))
        .thenReturn(response);
    getProductLevel3RepositoryBean().getProductCountByBrand("brand");
    Mockito.verify(getProductClient())
        .getProductsCountByBrand(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void getProductCountByBrandTest_Exception() throws Exception {
    try {
      SimpleLongResponse value = new SimpleLongResponse(1L);
      GdnRestSingleResponse<SimpleLongResponse> response =
          new GdnRestSingleResponse<>(null, null, false, value, DEFAULT_REQUEST_ID);
      when(getProductClient().getProductsCountByBrand(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString()))
          .thenReturn(response);

      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        getProductLevel3RepositoryBean().getProductCountByBrand(BRAND);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(getProductClient())
        .getProductsCountByBrand(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString());
  }

  @Test
  public void test_updateResignBusinessPartnerItems() throws Exception {
    when(getProductClient()
        .updateResignMerchantItemsByMerchantCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString())).thenReturn(new GdnBaseRestResponse(true));
    getProductLevel3RepositoryBean().updateResignBusinessPartnerItems(BUSINESS_PARTNER_CODE);
    Mockito.verify(getProductClient())
        .updateResignMerchantItemsByMerchantCode(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void test_updateResignBusinessPartnerItems_failed() throws Exception {
    when(getProductClient()
        .updateResignMerchantItemsByMerchantCode(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString())).thenReturn(new GdnBaseRestResponse(false));
    try {
      getProductLevel3RepositoryBean().updateResignBusinessPartnerItems(BUSINESS_PARTNER_CODE);
    } catch (Exception e) {
      Mockito.verify(getProductClient())
          .updateResignMerchantItemsByMerchantCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString());
    }
  }

  @Test
  public void test_getItemSkusByItemCode() throws Exception {
    when(getProductClient().getItemSkusByItemCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(ITEM_CODE)))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(0, 0, 0), REQUEST_ID));
    List<ItemPriceResponse> itemSkusToFetchEstimatedPrice =
        getProductLevel3RepositoryBean().getItemSkusToFetchEstimatedPrice(ITEM_CODE);
    Mockito.verify(getProductClient())
        .getItemSkusByItemCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.eq(ITEM_CODE));
    Assertions.assertNotNull(itemSkusToFetchEstimatedPrice);
  }

  @Test
  public void test_getItemSkusByItemCode_failed() throws Exception {
    when(getProductClient().getItemSkusByItemCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.eq(ITEM_CODE)))
        .thenReturn(new GdnRestListResponse<>());
    try {
      getProductLevel3RepositoryBean().getItemSkusToFetchEstimatedPrice(ITEM_CODE);
    } catch (Exception e) {
      Mockito.verify(getProductClient())
          .getItemSkusByItemCode(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.eq(ITEM_CODE));
    }
  }

  @Test
  public void getAllProducts_exceptionTest() throws Exception {
    when(getProductClient()
        .getAllProducts(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
            any(ActiveProductRequest.class))).thenReturn(new GdnRestListResponse<>());
    try {
      Page<ActiveProductResponse> activeProductResponseList = getProductLevel3RepositoryBean()
          .getAllProducts(summaryFilterRequest, PageRequest.of(PAGE_NUMBER, PAGE_SIZE), new ArrayList<>(),
              CATALOG_CODE, true);
    } catch (Exception e) {
      Mockito.verify(getProductClient())
          .getAllProducts(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.eq(PAGE_NUMBER),
              Mockito.eq(PAGE_SIZE), any());
    }
  }
  @Test
  public void getAllProductsTest() throws Exception {
    when(getProductClient()
        .getAllProducts(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
            any(ActiveProductRequest.class))).thenReturn(
        new GdnRestListResponse<>(new ArrayList<ActiveProductResponse>(), new PageMetaData(PAGE_SIZE, PAGE_NUMBER, 0),
            REQUEST_ID));
    Page<ActiveProductResponse> activeProductResponseList = getProductLevel3RepositoryBean()
        .getAllProducts(summaryFilterRequest, PageRequest.of(PAGE_NUMBER, PAGE_SIZE), new ArrayList<>(), CATALOG_CODE, true);
    Mockito.verify(getProductClient())
        .getAllProducts(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.eq(PAGE_NUMBER),
            Mockito.eq(PAGE_SIZE), any());
    Assertions.assertNotNull(activeProductResponseList);
  }

  @Test
  public void getSuspendedItemListTest() throws Exception {
    when(getProductClient()
        .getSuspendedItemList(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
            any(ActiveProductRequest.class))).thenReturn(
        new GdnRestListResponse<>(new ArrayList<ItemSummaryResponse>(), new PageMetaData(PAGE_SIZE, PAGE_NUMBER, 0),
            REQUEST_ID));
    Page<ItemSummaryResponse> itemSummaryResponseList = getProductLevel3RepositoryBean()
        .getSuspendedItemList(summaryFilterRequest, PageRequest.of(PAGE_NUMBER, PAGE_SIZE), new ArrayList<>(),
            CATALOG_CODE, true);
    Mockito.verify(getProductClient())
        .getSuspendedItemList(Mockito.anyString(), any(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.eq(PAGE_NUMBER),
            Mockito.eq(PAGE_SIZE), any());
    Assertions.assertNotNull(itemSummaryResponseList);
  }

  @Test
  public void getSuspendedItemList_exceptionTest() throws Exception {
    when(getProductClient()
        .getSuspendedItemList(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
            any(ActiveProductRequest.class))).thenReturn(new GdnRestListResponse<>());
    try {
      Page<ItemSummaryResponse> itemSummaryResponseList = getProductLevel3RepositoryBean()
          .getSuspendedItemList(summaryFilterRequest, PageRequest.of(PAGE_NUMBER, PAGE_SIZE), new ArrayList<>(),
              CATALOG_CODE, true);
    } catch (Exception e) {
      Mockito.verify(getProductClient())
          .getSuspendedItemList(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
              any(ActiveProductRequest.class));
    }
  }

  @Test
  public void getProductByProductCodeAndMerchantCodeTest() throws Exception {
    when(getProductClient()
        .getProductsByProductCodeAndMerchantCode(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(
        new GdnRestListResponse<ProductResponse>(Arrays.asList(new ProductResponse()), new PageMetaData(), requestId));
    List<ProductResponse> productResponse = getProductLevel3RepositoryBean()
        .getProductsByProductCodeAndMerchantCode(PRODUCT_CODE , BUSINESS_PARTNER_CODE);
    Mockito.verify(getProductClient())
        .getProductsByProductCodeAndMerchantCode(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(Constants.DEFAULT_USERNAME),
          Mockito.eq(PRODUCT_CODE),
            Mockito.eq(BUSINESS_PARTNER_CODE));
    Assertions.assertNotNull(productResponse);
  }

  @Test
  public void getProductByProductCodeAndMerchantCode_whenSuccessFalseTest() throws Exception {
    when(getProductClient()
        .getProductsByProductCodeAndMerchantCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(
        new GdnRestListResponse<ProductResponse>(ErrorCategory.UNSPECIFIED.getMessage(),
            ErrorCategory.UNSPECIFIED.getCode(), false, requestId));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductLevel3RepositoryBean().getProductsByProductCodeAndMerchantCode(PRODUCT_CODE, BUSINESS_PARTNER_CODE);
      });
    } finally {
      Mockito.verify(getProductClient())
          .getProductsByProductCodeAndMerchantCode(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_USERNAME),
              Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_CODE));
    }
  }

  @Test
  public void getProductByProductCodeAndMerchantCodeExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(getProductClient()).
        getProductsByProductCodeAndMerchantCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.eq(DEFAULT_USERNAME),
            Mockito.eq(PRODUCT_CODE), Mockito.eq(BUSINESS_PARTNER_CODE));
    try {
      List<ProductResponse> productResponse =
          getProductLevel3RepositoryBean().getProductsByProductCodeAndMerchantCode(PRODUCT_CODE, BUSINESS_PARTNER_CODE);
    } catch (Exception e) {
      Mockito.verify(getProductClient())
        .getProductsByProductCodeAndMerchantCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),
          Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(PRODUCT_CODE),
          Mockito.eq(BUSINESS_PARTNER_CODE));
    }
  }

  @Test
  public void testCampaignEligibilityForProductSku_Success() throws ApplicationException {
    PromoEligibilityRequest request = new PromoEligibilityRequest();
    PromoEligibilityResponse response = new PromoEligibilityResponse();
    Map<String, Boolean> eligibilityMap = new HashMap<>();
    eligibilityMap.put("sku1", true);
    response.setPromoEligibility(eligibilityMap);

    GdnRestSingleResponse<PromoEligibilityResponse> promoEligibilityResponse = new GdnRestSingleResponse<>();
    promoEligibilityResponse.setSuccess(true);
    promoEligibilityResponse.setValue(response);
    when(xProductFeign.getCampaignEligibilityForProductSkus(Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),
        Mockito.anyString(),Mockito.any())).thenReturn(promoEligibilityResponse);
    Map<String, Boolean> result =
        productLevel3RepositoryBean.campaignEligibilityForProductSku(request);
    Mockito.verify(xProductFeign)
        .getCampaignEligibilityForProductSkus(any(), any(), any(), any(), any(), any());
    assertEquals(eligibilityMap, result);
  }

  @Test
  public void testCampaignEligibilityForProductSku_Failure() throws ApplicationException {
    PromoEligibilityRequest request = new PromoEligibilityRequest();
    GdnRestSingleResponse<PromoEligibilityResponse> promoEligibilityResponse =
        new GdnRestSingleResponse<>();
    promoEligibilityResponse.setSuccess(false);
    promoEligibilityResponse.setErrorMessage("Error occurred");
    when(xProductFeign.getCampaignEligibilityForProductSkus(any(), any(), any(), any(), any(),
        any())).thenReturn(promoEligibilityResponse);
    Map<String, Boolean> result =
        productLevel3RepositoryBean.campaignEligibilityForProductSku(request);
    Mockito.verify(xProductFeign)
        .getCampaignEligibilityForProductSkus(any(), any(), any(), any(), any(), any());
    assertTrue(result.isEmpty());
  }

  @Test
  public void testCampaignEligibilityForProductSku_NullValue() throws ApplicationException {
    PromoEligibilityRequest request = new PromoEligibilityRequest();
    GdnRestSingleResponse<PromoEligibilityResponse> promoEligibilityResponse =
        new GdnRestSingleResponse<>();
    promoEligibilityResponse.setSuccess(true);
    promoEligibilityResponse.setValue(null);
    when(xProductFeign.getCampaignEligibilityForProductSkus(any(), any(), any(), any(), any(),
        any())).thenReturn(promoEligibilityResponse);
    Map<String, Boolean> result =
        productLevel3RepositoryBean.campaignEligibilityForProductSku(request);
    Mockito.verify(xProductFeign)
        .getCampaignEligibilityForProductSkus(any(), any(), any(), any(), any(), any());
    assertTrue(result.isEmpty());
  }
}
