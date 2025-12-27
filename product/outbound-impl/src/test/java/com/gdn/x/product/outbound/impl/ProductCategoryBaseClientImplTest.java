package com.gdn.x.product.outbound.impl;

import static com.gdn.x.product.enums.ProductFieldNames.ATTRIBUTE_CODE;
import static com.gdn.x.product.enums.ProductFieldNames.BRAND;
import static com.gdn.x.product.enums.ProductFieldNames.LOCATION_PATH;
import static com.gdn.x.product.enums.ProductFieldNames.SIZE_CHART_CODE;
import static com.gdn.x.product.enums.ProductFieldNames.UPC_CODE;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.dto.ListHolderRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryEligibleForSizeChartResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleListStringResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.outbound.api.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingWeightResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;

;

public class ProductCategoryBaseClientImplTest {

  private static ProductDetailResponse getProductResponse() {
    ProductDetailResponse response = new ProductDetailResponse();
    response.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    ProductCategoryResponse categoryResponse = new ProductCategoryResponse();
    categoryResponse.setId(ProductCategoryBaseClientImplTest.CATEGORY_ID2);
    categoryResponse.setCategory(new CategoryResponse());
    categoryResponse.getCategory().setId(ProductCategoryBaseClientImplTest.CATEGORY_ID2);
    response.getProductCategoryResponses().add(categoryResponse);
    return response;
  }

  private static final String CATEGORY_ID2 = "category-id";

  private static final String CATEGORY_CODE = "category-code";

  private static final String CATEGORY_ID = ProductCategoryBaseClientImplTest.CATEGORY_ID2;

  private static final String PRODUCT_CODE = "product-code";

  private static final String ITEM_CODE = "item-code";

  private static final String USER_NAME = "user-name";

  private static final String REQUEST_ID = "request-id";
  private static final String PCB_REQUEST_ID = "requestId";

  private static final String X_PRODUCT_CLIENT_ID = "x-product";
  private static final String X_PRODUCT_CHANNEL_ID = "x-product";
  private static final String STORE_ID = "store-id";
  private static final String ITEM_SKU = "itemSku";
  private static final List PRODUCT_CODES = Arrays.asList("PROD1", "PROD2");


  private static final GdnRestSingleResponse<ProductItemDetailResponse> PRODUCT_ITEM_DETAIL_RESPONSE =
      new GdnRestSingleResponse<ProductItemDetailResponse>(new ProductItemDetailResponse(),
          ProductCategoryBaseClientImplTest.REQUEST_ID);


  private static final GdnRestSingleResponse<ProductDetailResponse> PRODUCT_DETAIL_RESPONSE =
      new GdnRestSingleResponse<ProductDetailResponse>(getProductResponse(),
          ProductCategoryBaseClientImplTest.REQUEST_ID);

  @InjectMocks
  private ProductCategoryBaseOutboundImpl productCategoryBaseClient;

  @Mock
  private PCBFeign pcbFeign;

  private CategoryMultipleIdRequest categoryMultipleIdRequest;


  @Test
  public void getCategoryDetailSuccess() throws Exception {
    when(this.pcbFeign.getCategoryDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
        ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<CategoryDetailResponse>());
    this.productCategoryBaseClient
        .getCategoryDetail(ProductCategoryBaseClientImplTest.REQUEST_ID, ProductCategoryBaseClientImplTest.USER_NAME,
            ProductCategoryBaseClientImplTest.CATEGORY_ID);
    verify(this.pcbFeign).getCategoryDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
        ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.CATEGORY_ID);
  }

  @Test
  public void getCategoryDetailWithException() throws Exception {
    doThrow(RuntimeException.class).when(this.pcbFeign)
        .getCategoryDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
            ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.CATEGORY_ID);
    this.productCategoryBaseClient.getCategoryDetail(ProductCategoryBaseClientImplTest.REQUEST_ID,
        ProductCategoryBaseClientImplTest.USER_NAME, ProductCategoryBaseClientImplTest.CATEGORY_ID);
    verify(this.pcbFeign).getCategoryDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
        ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.CATEGORY_ID);
  }


  @Test
  public void getCategoryHierarchySuccess() throws Exception {
    when(this.pcbFeign.filterCategoryHierarchyByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
        ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.CATEGORY_CODE)).thenReturn(
        new GdnRestListResponse<CategoryResponse>(null, null, ProductCategoryBaseClientImplTest.REQUEST_ID));
    this.productCategoryBaseClient
        .getCategoryHierarchy(ProductCategoryBaseClientImplTest.REQUEST_ID, ProductCategoryBaseClientImplTest.USER_NAME,
            ProductCategoryBaseClientImplTest.CATEGORY_CODE);
    verify(this.pcbFeign)
        .filterCategoryHierarchyByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
            ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.CATEGORY_CODE);
  }

  @Test
  public void getCategoryHierarchyWithException() throws Exception {
    doThrow(RuntimeException.class).when(this.pcbFeign)
        .filterCategoryHierarchyByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
            ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.CATEGORY_CODE);
    List<CategoryResponse> categoryResponse = this.productCategoryBaseClient
        .getCategoryHierarchy(ProductCategoryBaseClientImplTest.REQUEST_ID, ProductCategoryBaseClientImplTest.USER_NAME,
            ProductCategoryBaseClientImplTest.CATEGORY_CODE);
    verify(this.pcbFeign)
        .filterCategoryHierarchyByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
            ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.CATEGORY_CODE);
    assertNotNull(categoryResponse);
  }

  @Test
  public void getProductDetailByProductCodeExceptionOccurs() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(this.pcbFeign)
        .getProductDetailByProductCode(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
            anyBoolean(), anyBoolean());
    boolean exceptionThrown = false;
    ProductDetailResponse response = null;
    try {
      response =
          this.productCategoryBaseClient. getProductDetailByProductCode(
              ProductCategoryBaseClientImplTest.REQUEST_ID,
              ProductCategoryBaseClientImplTest.USER_NAME,
              ProductCategoryBaseClientImplTest.PRODUCT_CODE);
    } catch (Exception e) {
      exceptionThrown = true;
    }
    assertTrue(exceptionThrown);
    verify(this.pcbFeign)
        .getProductDetailByProductCode(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
            anyBoolean(), anyBoolean());
    assertNull(response);

  }

  @Test
  public void getProductDetailByProductCodeSuccess() throws Exception {
    when(this.pcbFeign
        .getProductDetailByProductCode(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
            anyBoolean(), anyBoolean())).thenReturn(ProductCategoryBaseClientImplTest.PRODUCT_DETAIL_RESPONSE);
    GdnRestSingleResponse<CategoryDetailResponse> value = new GdnRestSingleResponse<CategoryDetailResponse>();
    value.setValue(new CategoryDetailResponse());
    this.productCategoryBaseClient.getProductDetailByProductCode(ProductCategoryBaseClientImplTest.REQUEST_ID,
        ProductCategoryBaseClientImplTest.USER_NAME, ProductCategoryBaseClientImplTest.PRODUCT_CODE);
    verify(this.pcbFeign)
        .getProductDetailByProductCode(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
            anyBoolean(), anyBoolean());
  }

  @Test
  public void getCategoryDetailByCategoryCodeTest(){
    GdnRestSingleResponse<CategoryDetailResponse> categoryResponse = new GdnRestSingleResponse<>();
    categoryResponse.setSuccess(true);
    when(this.pcbFeign
        .getCategoryDetailByCategoryCode(anyString(), anyString(), anyString(), anyString(), anyString(), anyString()))
        .thenReturn(categoryResponse);
    this.productCategoryBaseClient.getCategoryDetailByCategoryCode(ProductCategoryBaseClientImplTest.REQUEST_ID,
        ProductCategoryBaseClientImplTest.USER_NAME, ProductCategoryBaseClientImplTest.CATEGORY_CODE);
    verify(this.pcbFeign).getCategoryDetailByCategoryCode(anyString(), anyString(), anyString(), anyString(), anyString(), anyString());
  }


  @Test
  public void getProductItemDetailByItemCodeExceptionOccurs() throws Exception {
    doThrow(RuntimeException.class).when(this.pcbFeign)
        .getProductItemDetailBySkuCode(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
            anyBoolean());
    try {
      ProductItemDetailResponse response = this.productCategoryBaseClient
          .getProductItemDetailByItemCode(ProductCategoryBaseClientImplTest.REQUEST_ID,
              ProductCategoryBaseClientImplTest.USER_NAME, ProductCategoryBaseClientImplTest.ITEM_CODE);
    } finally {
      verify(this.pcbFeign)
          .getProductItemDetailBySkuCode(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
              anyBoolean());
    }
  }

  @Test
  public void getProductItemDetailByItemCodeSuccess() throws Exception {
    when(this.pcbFeign
        .getProductItemDetailBySkuCode(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
            anyBoolean())).thenReturn(ProductCategoryBaseClientImplTest.PRODUCT_ITEM_DETAIL_RESPONSE);
    this.productCategoryBaseClient.getProductItemDetailByItemCode(ProductCategoryBaseClientImplTest.REQUEST_ID,
        ProductCategoryBaseClientImplTest.USER_NAME, ProductCategoryBaseClientImplTest.ITEM_CODE);
    verify(this.pcbFeign)
        .getProductItemDetailBySkuCode(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
            anyBoolean());

  }

  @Test
  public void getMasterParentCategoryResponseByProductCodeTest() throws Exception {
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode("10521");
    categoryResponses.add(categoryResponse);
    GdnRestListResponse<CategoryResponse> clientresponse =
        new GdnRestListResponse<>(null, null, true, categoryResponses, null,
            ProductCategoryBaseClientImplTest.REQUEST_ID);
    Mockito.when(this.pcbFeign
        .getMasterParentCategoriesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
            ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.PRODUCT_CODE))
        .thenReturn(clientresponse);
    List<CategoryResponse> response = this.productCategoryBaseClient
        .getMasterParentCategoryResponseByProductCode(ProductCategoryBaseClientImplTest.REQUEST_ID,
            ProductCategoryBaseClientImplTest.USER_NAME, ProductCategoryBaseClientImplTest.PRODUCT_CODE);
    Mockito.verify(this.pcbFeign)
        .getMasterParentCategoriesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
            ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.PRODUCT_CODE);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getMasterParentCategoryResponseByProductCode_ResponseNullTest() throws Exception {
    GdnRestListResponse<CategoryResponse> clientresponse =
        new GdnRestListResponse<>(null, null, false, null, null, ProductCategoryBaseClientImplTest.REQUEST_ID);
    Mockito.when(this.pcbFeign
        .getMasterParentCategoriesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
            ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.PRODUCT_CODE))
        .thenReturn(clientresponse);
    List<CategoryResponse> response = this.productCategoryBaseClient
        .getMasterParentCategoryResponseByProductCode(ProductCategoryBaseClientImplTest.REQUEST_ID,
            ProductCategoryBaseClientImplTest.USER_NAME, ProductCategoryBaseClientImplTest.PRODUCT_CODE);
    Mockito.verify(this.pcbFeign)
        .getMasterParentCategoriesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
            ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.PRODUCT_CODE);
    Assertions.assertNull(response);
  }

  @Test
  public void getMasterParentCategoryResponseByProductCodeExceptionTest() throws Exception {

    Mockito.doThrow(new RuntimeException()).when(this.pcbFeign)
        .getMasterParentCategoriesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
            ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.PRODUCT_CODE);
    List<CategoryResponse> response = this.productCategoryBaseClient
        .getMasterParentCategoryResponseByProductCode(ProductCategoryBaseClientImplTest.REQUEST_ID,
            ProductCategoryBaseClientImplTest.USER_NAME, ProductCategoryBaseClientImplTest.PRODUCT_CODE);
    Mockito.verify(this.pcbFeign)
        .getMasterParentCategoriesByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, ProductCategoryBaseClientImplTest.PCB_REQUEST_ID,
            ProductCategoryBaseClientImplTest.X_PRODUCT_CLIENT_ID, ProductCategoryBaseClientImplTest.PRODUCT_CODE);
    Assertions.assertTrue(CollectionUtils.isEmpty(response));
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsExceptionOccurs() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(this.pcbFeign)
        .getProductDetailByProductCodeInAllProducts(anyString(), anyString(), anyString(), anyString(), anyString(),
            anyString(), anyBoolean(), anyBoolean());
    boolean exceptionThrown = false;
    ProductDetailResponse response = null;
    try {
      response = this.productCategoryBaseClient
          .getProductDetailByProductCodeForAllProducts(ProductCategoryBaseClientImplTest.REQUEST_ID,
              ProductCategoryBaseClientImplTest.USER_NAME, ProductCategoryBaseClientImplTest.PRODUCT_CODE, true);
    } catch (Exception e) {
      exceptionThrown = true;
    }
    assertTrue(exceptionThrown);
    verify(this.pcbFeign)
        .getProductDetailByProductCodeInAllProducts(anyString(), anyString(), anyString(), anyString(), anyString(),
            anyString(), anyBoolean(), anyBoolean());
    assertNull(response);
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsSuccessTest() throws Exception {
    when(this.pcbFeign
        .getProductDetailByProductCodeInAllProducts(anyString(), anyString(), anyString(), anyString(), anyString(),
            anyString(), anyBoolean(), anyBoolean()))
        .thenReturn(ProductCategoryBaseClientImplTest.PRODUCT_DETAIL_RESPONSE);
    GdnRestSingleResponse<CategoryDetailResponse> value = new GdnRestSingleResponse<CategoryDetailResponse>();
    value.setValue(new CategoryDetailResponse());
    when(this.pcbFeign.getCategoryDetail(anyString(), anyString(), anyString(), anyString(), anyString(), anyString()))
        .thenReturn(value);
    this.productCategoryBaseClient
        .getProductDetailByProductCodeForAllProducts(ProductCategoryBaseClientImplTest.REQUEST_ID,
            ProductCategoryBaseClientImplTest.USER_NAME, ProductCategoryBaseClientImplTest.PRODUCT_CODE, true);
    verify(this.pcbFeign)
        .getProductDetailByProductCodeInAllProducts(anyString(), anyString(), anyString(), anyString(), anyString(),
            anyString(), anyBoolean(), anyBoolean());
    verify(this.pcbFeign)
        .getCategoryDetail(anyString(), anyString(), anyString(), anyString(), anyString(), anyString());
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsCategoryResponseNullFailureTest() throws Exception {
    try {
      ProductDetailResponse productDetailResponse = getProductResponse();
      productDetailResponse.setProductCategoryResponses(null);
      GdnRestSingleResponse<ProductDetailResponse> PRODUCT_DETAIL_RESPONSE =
          new GdnRestSingleResponse<ProductDetailResponse>(productDetailResponse,
              ProductCategoryBaseClientImplTest.REQUEST_ID);
      when(this.pcbFeign.getProductDetailByProductCodeInAllProducts(anyString(), anyString(), anyString(), anyString(),
          anyString(), anyString(), anyBoolean(), anyBoolean())).thenReturn(PRODUCT_DETAIL_RESPONSE);
      GdnRestSingleResponse<CategoryDetailResponse> value = new GdnRestSingleResponse<CategoryDetailResponse>();
      value.setValue(new CategoryDetailResponse());
      when(this.pcbFeign.getCategoryDetail(anyString(), anyString(), anyString(), anyString(), anyString(),
          anyString())).thenReturn(value);
      this.productCategoryBaseClient.getProductDetailByProductCodeForAllProducts(
          ProductCategoryBaseClientImplTest.REQUEST_ID, ProductCategoryBaseClientImplTest.USER_NAME,
          ProductCategoryBaseClientImplTest.PRODUCT_CODE, true);
    } catch (ApplicationRuntimeException ex) {
      Assertions.assertEquals(ex.getErrorMessage(),
          "System/data are in invalid state :category not found for product code : product-code");
    } finally {
      verify(this.pcbFeign).getProductDetailByProductCodeInAllProducts(anyString(), anyString(), anyString(),
          anyString(), anyString(), anyString(), anyBoolean(), anyBoolean());
    }
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsCategoryResponseNullFailure1Test() throws Exception {
    try {
      ProductDetailResponse productDetailResponse = getProductResponse();
      List<ProductCategoryResponse> productCategoryResponses = new ArrayList<>();
      productCategoryResponses.add(null);
      productDetailResponse.setProductCategoryResponses(productCategoryResponses);
      GdnRestSingleResponse<ProductDetailResponse> PRODUCT_DETAIL_RESPONSE =
          new GdnRestSingleResponse<ProductDetailResponse>(productDetailResponse,
              ProductCategoryBaseClientImplTest.REQUEST_ID);
      when(this.pcbFeign.getProductDetailByProductCodeInAllProducts(anyString(), anyString(), anyString(), anyString(),
          anyString(), anyString(), anyBoolean(), anyBoolean())).thenReturn(PRODUCT_DETAIL_RESPONSE);
      GdnRestSingleResponse<CategoryDetailResponse> value = new GdnRestSingleResponse<CategoryDetailResponse>();
      value.setValue(new CategoryDetailResponse());
      when(this.pcbFeign.getCategoryDetail(anyString(), anyString(), anyString(), anyString(), anyString(),
          anyString())).thenReturn(value);
      this.productCategoryBaseClient.getProductDetailByProductCodeForAllProducts(
          ProductCategoryBaseClientImplTest.REQUEST_ID, ProductCategoryBaseClientImplTest.USER_NAME,
          ProductCategoryBaseClientImplTest.PRODUCT_CODE, true);
    } catch (ApplicationRuntimeException ex) {
      Assertions.assertEquals(ex.getErrorMessage(),
          "System/data are in invalid state :category not found for product code : product-code");
    } finally {
      verify(this.pcbFeign).getProductDetailByProductCodeInAllProducts(anyString(), anyString(), anyString(),
          anyString(), anyString(), anyString(), anyBoolean(), anyBoolean());
    }
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    categoryMultipleIdRequest = new CategoryMultipleIdRequest();
    categoryMultipleIdRequest.setCategoryCode(Arrays.asList("CAT-CODE"));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.pcbFeign);
  }

  @Test
  public void getSalesCategoryMappingByProductCodeTest() throws Exception {
    Mockito.when(this.pcbFeign
        .getSalesCategoryMapping(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, PCB_REQUEST_ID, X_PRODUCT_CLIENT_ID,
            PRODUCT_CODE, false)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new ProductSalesCategoryMappingResponse(), REQUEST_ID));
    this.productCategoryBaseClient.getSalesCategoryMappingByProductCode(STORE_ID, REQUEST_ID, USER_NAME, PRODUCT_CODE, false);
    Mockito.verify(this.pcbFeign)
        .getSalesCategoryMapping(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, PCB_REQUEST_ID, X_PRODUCT_CLIENT_ID,
            PRODUCT_CODE, false);
  }

  @Test
  public void getSalesCategoryMappingByProductCode_exceptionTest() throws Exception {
    Mockito.when(this.pcbFeign
        .getSalesCategoryMapping(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, PCB_REQUEST_ID, X_PRODUCT_CLIENT_ID,
            PRODUCT_CODE, true)).thenThrow(RuntimeException.class);
    this.productCategoryBaseClient.getSalesCategoryMappingByProductCode(STORE_ID, REQUEST_ID, USER_NAME, PRODUCT_CODE, true);
    Mockito.verify(this.pcbFeign)
        .getSalesCategoryMapping(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, PCB_REQUEST_ID, X_PRODUCT_CLIENT_ID,
            PRODUCT_CODE, true);
  }

  @Test
  public void generateShippingWeightTest() {
    Mockito.when(this.pcbFeign
        .generateShippingWeight(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CHANNEL_ID, CATEGORY_CODE, 0, 0, 0, 0))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new CategoryShippingWeightResponse(), REQUEST_ID));
    this.productCategoryBaseClient.generateShippingWeight(STORE_ID, CATEGORY_CODE, 0, 0, 0, 0);
    Mockito.verify(this.pcbFeign)
        .generateShippingWeight(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CHANNEL_ID, CATEGORY_CODE, 0, 0, 0, 0);
  }

  @Test
  public void generateShippingWeightExceptionTest() {
    Mockito.when(this.pcbFeign
        .generateShippingWeight(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CHANNEL_ID, CATEGORY_CODE, 0, 0, 0, 0)).thenThrow(RuntimeException.class);
    this.productCategoryBaseClient.generateShippingWeight(STORE_ID, CATEGORY_CODE, 0, 0, 0, 0);
    Mockito.verify(this.pcbFeign)
        .generateShippingWeight(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CHANNEL_ID, CATEGORY_CODE, 0, 0, 0, 0);
  }

  @Test
  public void generateShippingWeightNullValueExceptionTest() {
    Mockito.when(this.pcbFeign
        .generateShippingWeight(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CHANNEL_ID, CATEGORY_CODE, 0, 0, 0, 0))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    this.productCategoryBaseClient.generateShippingWeight(STORE_ID, CATEGORY_CODE, 0, 0, 0, 0);
    Mockito.verify(this.pcbFeign)
        .generateShippingWeight(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CHANNEL_ID, CATEGORY_CODE, 0, 0, 0, 0);
  }

  @Test
  public void generateShippingWeightNullResponseTest() {
    Mockito.when(this.pcbFeign
        .generateShippingWeight(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CHANNEL_ID, CATEGORY_CODE, 0, 0, 0, 0))
        .thenReturn(null);
    this.productCategoryBaseClient.generateShippingWeight(STORE_ID, CATEGORY_CODE, 0, 0, 0, 0);
    Mockito.verify(this.pcbFeign)
        .generateShippingWeight(STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CHANNEL_ID, CATEGORY_CODE, 0, 0, 0, 0);
  }

  @Test
  public void getCategoryNamesTest() {
    Mockito.when(this.pcbFeign
        .getCategoryNames(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
            eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            eq(Constants.DEFAULT_USERNAME), eq(0), eq(1),
            Mockito.any(CategoryMultipleIdRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new CategoryNamesResponse(), REQUEST_ID));
    CategoryNamesResponse categoryNamesResponse =
        this.productCategoryBaseClient.getCategoryNames(categoryMultipleIdRequest.getCategoryCode());
    Mockito.verify(this.pcbFeign)
        .getCategoryNames(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
            eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            eq(Constants.DEFAULT_USERNAME), eq(0), eq(1),
            Mockito.any(CategoryMultipleIdRequest.class));
    Assertions.assertNotNull(categoryNamesResponse);
    Assertions.assertNotNull(categoryNamesResponse.getCategoryMap());
  }

  @Test
  public void getCategoryNamesExceptionTest() {
    Mockito.doThrow(RuntimeException.class).when(this.pcbFeign)
        .getCategoryNames(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
            eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            eq(Constants.DEFAULT_USERNAME), eq(0), eq(1),
            Mockito.any(CategoryMultipleIdRequest.class));
    CategoryNamesResponse categoryNamesResponse =
        this.productCategoryBaseClient.getCategoryNames(categoryMultipleIdRequest.getCategoryCode());
    Mockito.verify(this.pcbFeign)
        .getCategoryNames(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
            eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            eq(Constants.DEFAULT_USERNAME), eq(0), eq(1),
            Mockito.any(CategoryMultipleIdRequest.class));
    Assertions.assertNull(categoryNamesResponse);
  }

  @Test
  public void getCnCategoryCodesTest() {
    Mockito.when(this.pcbFeign
        .getAllChildCategoriesFromC1CategoryCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
            eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            eq(Constants.DEFAULT_USERNAME), Mockito.any(CategoryCodeRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    List<String> categoryCodes =
        this.productCategoryBaseClient.getCnCategoryCodes(categoryMultipleIdRequest.getCategoryCode());
    Mockito.verify(this.pcbFeign)
        .getAllChildCategoriesFromC1CategoryCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
            eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            eq(Constants.DEFAULT_USERNAME), Mockito.any(CategoryCodeRequest.class));
    Assertions.assertNull(categoryCodes);
  }

  @Test
  public void getCnCategoryCodesNonNullTest() {
    CategoryCodeResponse categoryCodeResponse = new CategoryCodeResponse();
    categoryCodeResponse.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    Mockito.when(this.pcbFeign
        .getAllChildCategoriesFromC1CategoryCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
            eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            eq(Constants.DEFAULT_USERNAME), Mockito.any(CategoryCodeRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryCodeResponse, REQUEST_ID));
    List<String> categoryCodes =
        this.productCategoryBaseClient.getCnCategoryCodes(categoryMultipleIdRequest.getCategoryCode());
    Mockito.verify(this.pcbFeign)
        .getAllChildCategoriesFromC1CategoryCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
            eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            eq(Constants.DEFAULT_USERNAME), Mockito.any(CategoryCodeRequest.class));
    Assertions.assertNotNull(categoryCodes);
  }

  @Test
  public void getCnCategoryCodesNull1Test() {
    CategoryCodeResponse categoryCodeResponse = new CategoryCodeResponse();
    categoryCodeResponse.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    Mockito.when(this.pcbFeign.getAllChildCategoriesFromC1CategoryCode(eq(Constants.DEFAULT_STORE_ID),
        eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
        eq(Constants.DEFAULT_USERNAME), Mockito.any(CategoryCodeRequest.class))).thenReturn(null);
    List<String> categoryCodes =
        this.productCategoryBaseClient.getCnCategoryCodes(categoryMultipleIdRequest.getCategoryCode());
    Mockito.verify(this.pcbFeign).getAllChildCategoriesFromC1CategoryCode(eq(Constants.DEFAULT_STORE_ID),
        eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
        eq(Constants.DEFAULT_USERNAME), Mockito.any(CategoryCodeRequest.class));
    Assertions.assertNull(categoryCodes);
  }

  @Test
  public void getCnCategoryCodesExceptionTest() {
    Mockito.doThrow(RuntimeException.class).when(this.pcbFeign)
        .getAllChildCategoriesFromC1CategoryCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
            eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            eq(Constants.DEFAULT_USERNAME), Mockito.any(CategoryCodeRequest.class));
    List<String> categoryCodes =
        this.productCategoryBaseClient.getCnCategoryCodes(categoryMultipleIdRequest.getCategoryCode());
    Mockito.verify(this.pcbFeign)
        .getAllChildCategoriesFromC1CategoryCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
            eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
            eq(Constants.DEFAULT_USERNAME), Mockito.any(CategoryCodeRequest.class));
    Assertions.assertNull(categoryCodes);
  }

  @Test
  public void getMasterDataForTransactionTest() throws ApplicationException {
    Mockito.when(
        this.pcbFeign.getMasterProductDetailsByItemCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
          Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ITEM_CODE))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, new ProductMasterDataResponse(), REQUEST_ID));
    productCategoryBaseClient.getMasterDataForTransaction(ITEM_CODE);
    Mockito.verify(this.pcbFeign)
      .getMasterProductDetailsByItemCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ITEM_CODE);
  }

  @Test
  public void getMasterDataForTransaction_successFalseTest() throws ApplicationException {
    Mockito.when(
        this.pcbFeign.getMasterProductDetailsByItemCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
          Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ITEM_CODE))
      .thenReturn(new GdnRestSingleResponse<>(null, null, false, new ProductMasterDataResponse(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> productCategoryBaseClient.getMasterDataForTransaction(ITEM_CODE));
    } finally {
      Mockito.verify(this.pcbFeign)
        .getMasterProductDetailsByItemCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
          Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ITEM_CODE);
    }
  }

  @Test
  public void getMasterDataForTransaction_responseNullTest() throws ApplicationException {
    Mockito.when(
        this.pcbFeign.getMasterProductDetailsByItemCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
          Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ITEM_CODE))
      .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> productCategoryBaseClient.getMasterDataForTransaction(ITEM_CODE));
    } finally {
      Mockito.verify(this.pcbFeign)
        .getMasterProductDetailsByItemCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
          Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ITEM_CODE);
    }
  }

  @Test
  public void findProductItemDetailsBySkuCodesTest() throws Exception {
    List<String> skuCodes = new ArrayList<>();
    GdnRestListResponse<ProductItemDetailResponse> response = new GdnRestListResponse<>();
    List<ProductItemDetailResponse> itemDetailResponses = new ArrayList<>();
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setSkuCode(ITEM_SKU);
    productItemDetailResponse.setUpcCode(UPC_CODE);
    response.setContent(Collections.singletonList(productItemDetailResponse));
    skuCodes.add(ITEM_SKU);
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setSkuCodes(skuCodes);
    skuCodesRequest.setFetchArchived(false);
    Mockito.when(
      pcbFeign.filterProductItemBySkuCodes(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, skuCodesRequest)).thenReturn(response);
    List<ProductItemDetailResponse> productItemDetails =
      productCategoryBaseClient.findProductItemDetailsBySkuCodes(skuCodes, false, false);
    verify(pcbFeign).filterProductItemBySkuCodes(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
      Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, skuCodesRequest);
    Assertions.assertNotNull(productItemDetails);
  }

  @Test
  public void findProductItemDetailsBySkuCodesExceptionTest() throws Exception {
    List<String> skuCodes = new ArrayList<>();
    List<ProductItemDetailResponse> productItemDetails = new ArrayList<>();
    GdnRestListResponse<ProductItemDetailResponse> response = new GdnRestListResponse<>();
    List<ProductItemDetailResponse> itemDetailResponses = new ArrayList<>();
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setSkuCode(ITEM_SKU);
    productItemDetailResponse.setUpcCode(UPC_CODE);
    response.setContent(Collections.singletonList(productItemDetailResponse));
    skuCodes.add(ITEM_SKU);
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setSkuCodes(skuCodes);
    skuCodesRequest.setFetchArchived(false);
    doThrow(RuntimeException.class).when(pcbFeign).filterProductItemBySkuCodes(Constants.DEFAULT_STORE_ID,
      X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
      Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, skuCodesRequest);
    try {
        productItemDetails = productCategoryBaseClient.findProductItemDetailsBySkuCodes(skuCodes,
        false, false);
    }
    finally {
      verify(pcbFeign).filterProductItemBySkuCodes(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false, skuCodesRequest);
      Assertions.assertTrue(CollectionUtils.isEmpty(productItemDetails));
    }
  }

  @Test
  public void findProductItemDetailsBySkuCodesNullResponseTest() throws Exception {
    List<String> skuCodes = new ArrayList<>();
    GdnRestListResponse<ProductItemDetailResponse> response = null;
    List<ProductItemDetailResponse> productItemDetails = null;
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setSkuCode(ITEM_SKU);
    productItemDetailResponse.setUpcCode(UPC_CODE);
    skuCodes.add(ITEM_SKU);
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setSkuCodes(skuCodes);
    skuCodesRequest.setFetchArchived(false);
    Mockito.when(
      pcbFeign.filterProductItemBySkuCodes(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
        X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false,
        skuCodesRequest)).thenReturn(null);
     productItemDetails =  productCategoryBaseClient.findProductItemDetailsBySkuCodes(skuCodes,
      false, false);
    verify(pcbFeign).filterProductItemBySkuCodes(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
      X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, false,
      skuCodesRequest);
    Assertions.assertTrue(CollectionUtils.isEmpty(productItemDetails));
  }

  @Test
  public void filterProductImagesByProductCodesTest() throws Exception {
    ListHolderRequest<String> request = new ListHolderRequest<>();
    GdnRestListResponse<ImageResponse> response = new GdnRestListResponse();
    response.setContent(Arrays.asList(new ImageResponse()));
    List<ProductImageResponse> productImageResponses;
    Mockito.when(
        pcbFeign.getProductImagesByProductCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID),
            eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), anyString())).thenReturn(response);
    List<ImageResponse> imageResponseList = productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getProductImagesByProductCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID),
        eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), anyString());
    Assertions.assertNotNull(imageResponseList);
  }

  @Test
  public void getproductImagesByProductCodeExceptionTest() throws Exception {
    ListHolderRequest<String> request = new ListHolderRequest<>();
    request.setLists(Arrays.asList(PRODUCT_CODE));
    doThrow(RuntimeException.class).when(pcbFeign).getProductImagesByProductCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
        eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), anyString());
    List<ProductImageResponse> productImageResponses = new ArrayList<>();
    try {
      List<ImageResponse> imageResponseList = productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE);
    }
    finally {
      verify(pcbFeign).getProductImagesByProductCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID),
          eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), anyString());
      Assertions.assertTrue(CollectionUtils.isEmpty(productImageResponses));
    }
  }

  @Test
  public void filterProductImagesByProductCodesNullResponseTest() throws Exception {
    ListHolderRequest<String> request = new ListHolderRequest<>();
    GdnRestListResponse<ImageResponse> response = null;
    Mockito.when(
        pcbFeign.getProductImagesByProductCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID),
            eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), anyString())).thenReturn(response);
    List<ImageResponse> imageResponseList = productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getProductImagesByProductCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID),
        eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), anyString());
    Assertions.assertNotNull(imageResponseList);
    Assertions.assertTrue(CollectionUtils.isEmpty(imageResponseList));
  }

  @Test
  public void getProductItemImagesByItemCodeTest() throws Exception {
    GdnRestListResponse<ItemImageResponse> response = new GdnRestListResponse();
    response.setContent(Arrays.asList(new ItemImageResponse(ITEM_CODE, UPC_CODE, ITEM_CODE, Arrays.asList(new ImageResponse()))));
    Mockito.when(
        pcbFeign.getProductItemImagesByItemCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID),
            eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), any())).thenReturn(response);
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setSkuCodes(Collections.singletonList(ITEM_CODE));
    GdnRestListResponse<ItemImageResponse> productItemImagesByItemCode =
      productCategoryBaseClient.getProductItemImagesByItemCode(skuCodesRequest);
    verify(pcbFeign).getProductItemImagesByItemCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID),
        eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), any());
    Assertions.assertNotNull(productItemImagesByItemCode.getContent());
  }

  @Test
  public void getProductItemImagesByItemCodeExceptionTest() throws Exception {
    doThrow(RuntimeException.class).when(pcbFeign).getProductItemImagesByItemCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
        eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), any());
    List<ProductImageResponse> productImageResponses = new ArrayList<>();
    try {
      SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
      skuCodesRequest.setSkuCodes(Collections.singletonList(ITEM_CODE));
      GdnRestListResponse<ItemImageResponse> itemImagesByItemCode =
        productCategoryBaseClient.getProductItemImagesByItemCode(skuCodesRequest);
    }
    finally {
      verify(pcbFeign).getProductItemImagesByItemCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID),
          eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), any());
    }
  }

  @Test
  public void getProductItemImagesByItemCodeNullResponseTest() throws Exception {
    ListHolderRequest<String> request = new ListHolderRequest<>();
    GdnRestListResponse<ItemImageResponse> response = null;
    Mockito.when(
        pcbFeign.getProductItemImagesByItemCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID),
            eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), any())).thenReturn(response);
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setSkuCodes(Collections.singletonList(ITEM_CODE));
    GdnRestListResponse<ItemImageResponse> productItemImagesByItemCode =
      productCategoryBaseClient.getProductItemImagesByItemCode(skuCodesRequest);
    verify(pcbFeign).getProductItemImagesByItemCode(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID),
        eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME), any());
    Assertions.assertNull(productItemImagesByItemCode);
  }

  @Test
  public void getAttributeDetailByAttributeCodeExceptionSucessFalseTest() throws Exception{
    Mockito.when(this.pcbFeign.getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ATTRIBUTE_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, new AttributeResponse(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productCategoryBaseClient.getAttributeDetailByAttributeCode(ATTRIBUTE_CODE));
    } finally {
      Mockito.verify(this.pcbFeign)
          .getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ATTRIBUTE_CODE);
    }
  }

  @Test
  public void getAttributeDetailByAttributeCodeExceptionValueNullTest() throws Exception{
    Mockito.when(this.pcbFeign.getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ATTRIBUTE_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productCategoryBaseClient.getAttributeDetailByAttributeCode(ATTRIBUTE_CODE));
    } finally {
      Mockito.verify(this.pcbFeign)
          .getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ATTRIBUTE_CODE);
    }
  }

  @Test
  public void getAttributeDetailByAttributeCodeExceptionSuccessFalseNullTest() throws Exception{
    Mockito.when(this.pcbFeign.getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ATTRIBUTE_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new AttributeResponse(), REQUEST_ID));
    try {
      productCategoryBaseClient.getAttributeDetailByAttributeCode(ATTRIBUTE_CODE);
    } finally {
      Mockito.verify(this.pcbFeign)
          .getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, ATTRIBUTE_CODE);
    }
  }

  @Test
  public void getProductBasicDetailsTest() throws Exception {
    GdnRestSingleResponse<ProductResponse> response = new GdnRestSingleResponse<>();
    response.setValue(new ProductResponse());
    response.setSuccess(true);
    Mockito.when(
        pcbFeign.getProductBasicDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE)).thenReturn(response);
    ProductResponse productResponse = productCategoryBaseClient.getProductBasicDetails(PRODUCT_CODE);
    verify(pcbFeign).getProductBasicDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
        X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    Assertions.assertNotNull(productResponse);
  }

  @Test
  public void getProductBasicDetailsExceptionTest() throws Exception {
    GdnRestSingleResponse<ProductResponse> response = new GdnRestSingleResponse<>();
    response.setValue(new ProductResponse());
    response.setSuccess(false);
    Mockito.when(
        pcbFeign.getProductBasicDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productCategoryBaseClient.getProductBasicDetails(PRODUCT_CODE));
    }
    finally {
      verify(pcbFeign).getProductBasicDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
          X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    }
  }

  @Test
  public void getProductBasicDetailsNullResponseTest() throws Exception {
    GdnRestSingleResponse<ProductResponse> response = null;
    Mockito.when(
        pcbFeign.getProductBasicDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE)).thenReturn(response);
    try {
      Assertions.assertThrows(Exception.class, () -> productCategoryBaseClient.getProductBasicDetails(PRODUCT_CODE));
    } finally {
      verify(pcbFeign).getProductBasicDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
          X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    }
  }

  @Test
  public void getProductAndAttributeDetailsTest() throws Exception {
    GdnRestSingleResponse<ProductAndAttributeDetailResponse> response = new GdnRestSingleResponse<>();
    response.setValue(new ProductAndAttributeDetailResponse());
    response.setSuccess(true);
    Mockito.when(
        pcbFeign.getProductAndAttributeDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,false)).thenReturn(response);
    ProductAndAttributeDetailResponse response1 = productCategoryBaseClient.getProductAndAttributeDetails(PRODUCT_CODE,
        false);
    verify(pcbFeign).getProductAndAttributeDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
        X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,false);
    Assertions.assertNotNull(response1);
  }

  @Test
  public void getProductAndAttributeDetailsExceptionTest() throws Exception {
    GdnRestSingleResponse<ProductAndAttributeDetailResponse> response = new GdnRestSingleResponse<>();
    Mockito.when(pcbFeign.getProductAndAttributeDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,false)).thenReturn(response);
    response.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productCategoryBaseClient.getProductAndAttributeDetails(PRODUCT_CODE,
          false));
    }
    finally {
      verify(pcbFeign).getProductAndAttributeDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
          X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,false);
    }
  }

  @Test
  public void getProductAndAttributeDetailsNullResponseTest() throws Exception {
    GdnRestSingleResponse<ProductAndAttributeDetailResponse> response = null;
    Mockito.when(
        pcbFeign.getProductAndAttributeDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
            X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,false)).thenReturn(response);
    try {
      Assertions.assertThrows(Exception.class, () -> productCategoryBaseClient.getProductAndAttributeDetails(PRODUCT_CODE,
          false));
    } finally {
      verify(pcbFeign).getProductAndAttributeDetails(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
          X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_CODE,false);
    }
  }

  @Test
  public void getBrandLogoUrlNullTest() {
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productCategoryBaseClient.getBrandLogoUrl(BRAND));
    } finally {
      Mockito.verify(pcbFeign).getBrandResponseByBrandCode(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), BRAND);
    }
  }

  @Test
  public void getBrandLogoUrlFalseTest() {
    Mockito.when(pcbFeign.getBrandResponseByBrandCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), BRAND))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productCategoryBaseClient.getBrandLogoUrl(BRAND));
    } finally {
      Mockito.verify(pcbFeign).getBrandResponseByBrandCode(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), BRAND);
    }
  }

  @Test
  public void getBrandLogoUrlResponseNullTest() {
    Mockito.when(pcbFeign.getBrandResponseByBrandCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), BRAND))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, null));
    String brandLogoUrl = productCategoryBaseClient.getBrandLogoUrl(BRAND);
    Mockito.verify(pcbFeign).getBrandResponseByBrandCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), BRAND);
    Assertions.assertTrue(StringUtils.isEmpty(brandLogoUrl));
  }

  @Test
  public void getBrandLogoUrlResponseNotNullTest() {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandLogoPath(LOCATION_PATH);
    Mockito.when(pcbFeign.getBrandResponseByBrandCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), BRAND))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, brandResponse, null));
    String brandLogoUrl = productCategoryBaseClient.getBrandLogoUrl(BRAND);
    Mockito.verify(pcbFeign).getBrandResponseByBrandCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), BRAND);
    Assertions.assertEquals(LOCATION_PATH, brandLogoUrl);
  }

  @Test
  public void fetchSizeChartDetailsTest() {
    SizeChartResponse sizeChartResponse = new SizeChartResponse();
    sizeChartResponse.setSizeChartCode(SIZE_CHART_CODE);
    Mockito.when(pcbFeign.fetchSizeChartDetails(STORE_ID, GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), true, SIZE_CHART_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, sizeChartResponse, null));
    productCategoryBaseClient.fetchSizeChartDetails(STORE_ID, SIZE_CHART_CODE);
    Mockito.verify(pcbFeign).fetchSizeChartDetails(STORE_ID, GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), true, SIZE_CHART_CODE);
    Assertions.assertEquals(SIZE_CHART_CODE, sizeChartResponse.getSizeChartCode());
  }

  @Test
  public void fetchSizeChartDetailsNullTest() {
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productCategoryBaseClient.fetchSizeChartDetails(STORE_ID, SIZE_CHART_CODE));
    } finally {
      Mockito.verify(pcbFeign).fetchSizeChartDetails(STORE_ID, GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), true, SIZE_CHART_CODE);
    }
  }

  @Test
  public void fetchSizeChartDetailsSuccessFalseTest() {
    SizeChartResponse sizeChartResponse = new SizeChartResponse();
    sizeChartResponse.setSizeChartCode(SIZE_CHART_CODE);
    Mockito.when(pcbFeign.fetchSizeChartDetails(STORE_ID, GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), true, SIZE_CHART_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, sizeChartResponse, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> productCategoryBaseClient.fetchSizeChartDetails(STORE_ID, SIZE_CHART_CODE));
    } finally {
      Mockito.verify(pcbFeign).fetchSizeChartDetails(STORE_ID, GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), true, SIZE_CHART_CODE);
      Assertions.assertEquals(SIZE_CHART_CODE, sizeChartResponse.getSizeChartCode());
    }
  }

  @Test
  public void isSizeChartCodeValidTrueTest() {
    Mockito.when(pcbFeign.validateSizeChartCode(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), SIZE_CHART_CODE))
        .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    boolean response = productCategoryBaseClient.isSizeChartCodeValid(SIZE_CHART_CODE);
    Mockito.verify(pcbFeign).validateSizeChartCode(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), SIZE_CHART_CODE);
    Assertions.assertTrue(response);
  }

  @Test
  public void isSizeChartCodeValidFalseTest() {
    Mockito.when(pcbFeign.validateSizeChartCode(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), SIZE_CHART_CODE))
        .thenReturn(new GdnBaseRestResponse(null, null, false, null));
    boolean response = productCategoryBaseClient.isSizeChartCodeValid(SIZE_CHART_CODE);
    Mockito.verify(pcbFeign).validateSizeChartCode(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), SIZE_CHART_CODE);
    Assertions.assertFalse(response);
  }

  @Test
  public void getCategoryAndEligibleFlagMapTest() {
    GdnRestSingleResponse<CategoryEligibleForSizeChartResponse> response = new GdnRestSingleResponse<>();
    CategoryEligibleForSizeChartResponse categoryEligibleForSizeChartResponse =
        new CategoryEligibleForSizeChartResponse();
    categoryEligibleForSizeChartResponse.setCategoryCodeAndEligibilityMap(Map.of(CATEGORY_CODE, true));
    response.setValue(categoryEligibleForSizeChartResponse);
    List<String> categoryCodes = Arrays.asList(CATEGORY_CODE);
    response.setSuccess(true);
    Mockito.when(pcbFeign.checkCategoryEligibleForSizeChartAddition(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), categoryCodes,
        ATTRIBUTE_CODE)).thenReturn(response);
    Map<String, Boolean> pcbResponse =
        productCategoryBaseClient.getCategoryAndEligibleFlagMap(GdnMandatoryRequestParameterUtil.getStoreId(),
            ATTRIBUTE_CODE, categoryCodes);
    Mockito.verify(pcbFeign).checkCategoryEligibleForSizeChartAddition(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), categoryCodes,
        ATTRIBUTE_CODE);
    Assertions.assertTrue(pcbResponse.get(CATEGORY_CODE));
  }

  @Test
  public void getCategoryAndEligibleFlagMapSuccessFalseTest() {
    GdnRestSingleResponse<CategoryEligibleForSizeChartResponse> response = new GdnRestSingleResponse<>();
    CategoryEligibleForSizeChartResponse categoryEligibleForSizeChartResponse =
        new CategoryEligibleForSizeChartResponse();
    categoryEligibleForSizeChartResponse.setCategoryCodeAndEligibilityMap(Map.of(CATEGORY_CODE, true));
    response.setValue(categoryEligibleForSizeChartResponse);
    List<String> categoryCodes = Arrays.asList(CATEGORY_CODE);
    response.setSuccess(false);
    Mockito.when(pcbFeign.checkCategoryEligibleForSizeChartAddition(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), categoryCodes,
        ATTRIBUTE_CODE)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          productCategoryBaseClient.getCategoryAndEligibleFlagMap(GdnMandatoryRequestParameterUtil.getStoreId(),
              ATTRIBUTE_CODE, categoryCodes));
    } finally {
      Mockito.verify(pcbFeign).checkCategoryEligibleForSizeChartAddition(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          categoryCodes, ATTRIBUTE_CODE);
    }
  }

  @Test
  public void getCategoryAndEligibleFlagMapTestNullResponse() {
    List<String> categoryCodes = Arrays.asList(CATEGORY_CODE);
    Mockito.when(pcbFeign.checkCategoryEligibleForSizeChartAddition(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), categoryCodes,
        ATTRIBUTE_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          productCategoryBaseClient.getCategoryAndEligibleFlagMap(GdnMandatoryRequestParameterUtil.getStoreId(),
              ATTRIBUTE_CODE, categoryCodes));
    } finally {
      Mockito.verify(pcbFeign).checkCategoryEligibleForSizeChartAddition(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          categoryCodes, ATTRIBUTE_CODE);
    }
  }

  @Test
  public void getCategoryCodesByAttributeCodeTest() {
    GdnRestSingleResponse<SimpleListStringResponse> response =
        new GdnRestSingleResponse<>(new SimpleListStringResponse(Arrays.asList(CATEGORY_CODE)),
            null);
    Mockito.when(
        pcbFeign.getCategoryCodesByAttributeCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), ATTRIBUTE_CODE)).thenReturn(response);
    List<String> categoryCodes =
        productCategoryBaseClient.getCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Assertions.assertFalse(categoryCodes.isEmpty());
    Mockito.verify(pcbFeign)
        .getCategoryCodesByAttributeCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), ATTRIBUTE_CODE);
  }

  @Test
  public void getCategoryCodesByAttributeCodeSuccessFalseTest() {
    GdnRestSingleResponse<SimpleListStringResponse> response =
        new GdnRestSingleResponse<>(null, null, false,
            new SimpleListStringResponse(Arrays.asList(CATEGORY_CODE)), null);
    Mockito.when(
        pcbFeign.getCategoryCodesByAttributeCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), ATTRIBUTE_CODE)).thenReturn(response);

    Assertions.assertThrows(RuntimeException.class,
        () -> productCategoryBaseClient.getCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE));

    Mockito.verify(pcbFeign)
        .getCategoryCodesByAttributeCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), ATTRIBUTE_CODE);
  }

  @Test
  public void getCategoryCodesByAttributeCodeSuccessTruendResponseNullTest() {
    GdnRestSingleResponse<SimpleListStringResponse> response =
        new GdnRestSingleResponse<>(null, null, true, null, null);
    Mockito.when(
        pcbFeign.getCategoryCodesByAttributeCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), ATTRIBUTE_CODE)).thenReturn(response);

    Assertions.assertThrows(RuntimeException.class,
        () -> productCategoryBaseClient.getCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE));

    Mockito.verify(pcbFeign)
        .getCategoryCodesByAttributeCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), ATTRIBUTE_CODE);
  }

  @Test
  public void updateStatusInPCBForBackFillAttributes_ShouldCallFeignClient_WhenSuccess() {
    String productCode = "P12345";
    String updatedStatus = "COMPLETED";
    String errorMessage = null;
    GdnBaseRestResponse baseRestResponse = new GdnBaseRestResponse();
    baseRestResponse.setSuccess(true);
    when(pcbFeign.updateProductMigrationStatus(anyString(), anyString(), any())).thenReturn(baseRestResponse);
    productCategoryBaseClient.updateStatusInPCBForBackFillAttributes(productCode, updatedStatus, errorMessage);
    verify(pcbFeign).updateProductMigrationStatus(anyString(), anyString(), any());
  }

  @Test
  public void updateStatusInPCBForBackFillAttributes_ShouldLogError_WhenFeignCallFails() {
    String productCode = "P67890";
    String updatedStatus = "FAILED";
    String errorMessage = "Some error";
    GdnBaseRestResponse baseRestResponse = new GdnBaseRestResponse();
    baseRestResponse.setSuccess(false);
    baseRestResponse.setErrorMessage("Internal Server Error");
    when(pcbFeign.updateProductMigrationStatus(anyString(), anyString(), any())).thenReturn(baseRestResponse);
    productCategoryBaseClient.updateStatusInPCBForBackFillAttributes(productCode, updatedStatus, errorMessage);
    verify(pcbFeign).updateProductMigrationStatus(anyString(), anyString(), any());
  }

  @Test
  void getBasicInfoProductDetailsListByProductCodes_Success() {
    List<BasicInfoProductResponse> content = new ArrayList<>();
    BasicInfoProductResponse product1 = new BasicInfoProductResponse();
    product1.setProductCode("PROD1");
    BasicInfoProductResponse product2 = new BasicInfoProductResponse();
    product2.setProductCode("PROD2");
    content.add(product1);
    content.add(product2);
    when(pcbFeign.getBasicInfoProductDetailsListByProductCodes(eq(Constants.DEFAULT_STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
        eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME),
        eq(PRODUCT_CODES))).thenReturn(
        new GdnRestListResponse<>(null, null, true, content, null, Constants.DEFAULT_REQUEST_ID));

    List<BasicInfoProductResponse> result =
        productCategoryBaseClient.getBasicInfoProductDetailsListByProductCodes(PRODUCT_CODES);

    assertNotNull(result);
    Assertions.assertEquals(2, result.size());
    Assertions.assertEquals("PROD1", result.get(0).getProductCode());
    Assertions.assertEquals("PROD2", result.get(1).getProductCode());
    verify(pcbFeign).getBasicInfoProductDetailsListByProductCodes(eq(Constants.DEFAULT_STORE_ID),
        eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
        eq(Constants.DEFAULT_USERNAME), eq(PRODUCT_CODES));
  }

  @Test
  void getBasicInfoProductDetailsListByProductCodes_EmptyList() {
    List<String> emptyProductCodes = Collections.emptyList();
    when(pcbFeign.getBasicInfoProductDetailsListByProductCodes(anyString(), anyString(), anyString(), anyString(),
        anyString(), eq(emptyProductCodes))).thenReturn(
        new GdnRestListResponse<>(null, null, true, Collections.emptyList(), null, Constants.DEFAULT_REQUEST_ID));

    List<BasicInfoProductResponse> result =
        productCategoryBaseClient.getBasicInfoProductDetailsListByProductCodes(emptyProductCodes);

    assertNotNull(result);
    assertTrue(result.isEmpty());
    verify(pcbFeign).getBasicInfoProductDetailsListByProductCodes(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID,
        X_PRODUCT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, emptyProductCodes);
  }

  @Test
  void getBasicInfoProductDetailsListByProductCodes_Exception() {
    when(pcbFeign.getBasicInfoProductDetailsListByProductCodes(anyString(), anyString(), anyString(), anyString(),
        anyString(), Mockito.anyList())).thenThrow(new RuntimeException("Test exception"));

    List<BasicInfoProductResponse> result =
        productCategoryBaseClient.getBasicInfoProductDetailsListByProductCodes(PRODUCT_CODES);

    assertNotNull(result);
    assertTrue(result.isEmpty());
    verify(pcbFeign).getBasicInfoProductDetailsListByProductCodes(eq(Constants.DEFAULT_STORE_ID),
        eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
        eq(Constants.DEFAULT_USERNAME), Mockito.anyList());
  }

  @Test
  void getBasicInfoProductDetailsListByProductCodes_NullResponse() {
    when(pcbFeign.getBasicInfoProductDetailsListByProductCodes(anyString(), anyString(), anyString(), anyString(),
        anyString(), Mockito.anyList())).thenReturn(null);

    List<BasicInfoProductResponse> result =
        productCategoryBaseClient.getBasicInfoProductDetailsListByProductCodes(PRODUCT_CODES);
    assertNotNull(result);
    assertTrue(result.isEmpty());
    verify(pcbFeign).getBasicInfoProductDetailsListByProductCodes(eq(Constants.DEFAULT_STORE_ID),
        eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
        eq(Constants.DEFAULT_USERNAME), Mockito.anyList());
  }

  @Test
  void getBasicInfoProductDetailsListByProductCodes_ErrorResponse() {
    when(pcbFeign.getBasicInfoProductDetailsListByProductCodes(anyString(), anyString(), anyString(), anyString(),
        anyString(), Mockito.anyList())).thenReturn(
        new GdnRestListResponse<>("Error occurred", null, false, null, null, Constants.DEFAULT_REQUEST_ID));

    List<BasicInfoProductResponse> result =
        productCategoryBaseClient.getBasicInfoProductDetailsListByProductCodes(PRODUCT_CODES);

    assertNotNull(result);
    assertTrue(result.isEmpty());
    verify(pcbFeign).getBasicInfoProductDetailsListByProductCodes(eq(Constants.DEFAULT_STORE_ID),
        eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID),
        eq(Constants.DEFAULT_USERNAME), Mockito.anyList());
  }

  @Test
  public void checkOmniChannelSkuExistsInSellerSuccessTest() throws ApplicationException {
    ValidOmniChannelSkuResponse validOmniChannelSkuResponse = new ValidOmniChannelSkuResponse();
    GdnRestSingleResponse<ValidOmniChannelSkuResponse> response = new GdnRestSingleResponse<>();
    response.setValue(validOmniChannelSkuResponse);
    response.setSuccess(true);

    when(pcbFeign.checkOmniChannelSkuExistsInSeller(anyString(), anyString(), anyString(), anyString(), anyString(),
        anyBoolean(), any())).thenReturn(response);

    OmniChannelSkuRequest request = new OmniChannelSkuRequest();
    ValidOmniChannelSkuResponse result =
        productCategoryBaseClient.checkOmniChannelSkuExistsInSeller(STORE_ID, REQUEST_ID, USER_NAME, true, request);

    verify(pcbFeign).checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(X_PRODUCT_CHANNEL_ID), eq(X_PRODUCT_CLIENT_ID),
        anyString(), anyString(), eq(true), any());
    assertNotNull(result);
  }

  @Test
  public void checkOmniChannelSkuExistsInSellerNullResponseTest() throws ApplicationException {
    when(pcbFeign.checkOmniChannelSkuExistsInSeller(anyString(), anyString(), anyString(), anyString(), anyString(),
        anyBoolean(), any())).thenReturn(null);

    OmniChannelSkuRequest request = new OmniChannelSkuRequest();
    try {
      productCategoryBaseClient.checkOmniChannelSkuExistsInSeller(STORE_ID, REQUEST_ID, USER_NAME, true, request);
    } catch (ApplicationException e) {
      verify(pcbFeign).checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
          eq(X_PRODUCT_CLIENT_ID), anyString(), anyString(), eq(true), any());
      assertNotNull(e);
    }
  }

  @Test
  public void checkOmniChannelSkuExistsInSellerUnsuccessfulResponseTest() throws ApplicationException {
    GdnRestSingleResponse<ValidOmniChannelSkuResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    response.setErrorMessage("Error message");

    when(pcbFeign.checkOmniChannelSkuExistsInSeller(anyString(), anyString(), anyString(), anyString(), anyString(),
        anyBoolean(), any())).thenReturn(response);

    OmniChannelSkuRequest request = new OmniChannelSkuRequest();
    try {
      productCategoryBaseClient.checkOmniChannelSkuExistsInSeller(STORE_ID, REQUEST_ID, USER_NAME, true, request);
    } catch (ApplicationException e) {
      verify(pcbFeign).checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
          eq(X_PRODUCT_CLIENT_ID), anyString(), anyString(), eq(true), any());
      assertNotNull(e);
    }
  }

  @Test
  public void checkOmniChannelSkuExistsInSellerExceptionTest() throws ApplicationException {
    when(pcbFeign.checkOmniChannelSkuExistsInSeller(anyString(), anyString(), anyString(), anyString(), anyString(),
        anyBoolean(), any())).thenThrow(new RuntimeException("Test exception"));

    OmniChannelSkuRequest request = new OmniChannelSkuRequest();
    try {
      productCategoryBaseClient.checkOmniChannelSkuExistsInSeller(STORE_ID, REQUEST_ID, USER_NAME, true, request);
    } catch (RuntimeException e) {
      verify(pcbFeign).checkOmniChannelSkuExistsInSeller(eq(STORE_ID), eq(X_PRODUCT_CHANNEL_ID),
          eq(X_PRODUCT_CLIENT_ID), anyString(), anyString(), eq(true), any());
      assertNotNull(e);
    }
  }
}
