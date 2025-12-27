package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.vo.MasterDataCacheVo;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public class PCBMasterDataServiceImplTest {

  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "request-id";

  private static final String PRODUCT_CODE = "product-code";

  private static final String BRAND_LOGO_URL = "brandLogoUrl";

  private static final String BRAND_CODE = "brandCode";

  private static final String EMPTY = "";

  private static final boolean IN_ALL_PRODUCTS = false;

  @InjectMocks
  private PCBMasterDataServiceImpl pcbMasterDataServiceImpl;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  private ProductDetailResponse productDetailResponse;

  private GdnRestSingleResponse<BrandResponse> brandResponse;

  private AttributeResponse attribute;

  private MasterDataProduct masterDataProduct;

  private ProductAttributeResponse productAttributeResponses;

  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();

  private List<ProductAttributeValueResponse> productAttributeValueResponseList =
      new ArrayList<ProductAttributeValueResponse>();

  private List<ProductAttributeResponse> productAttributeResponsesList = new ArrayList<>();

  private HashSet<ProductItemResponse> itemResponses = new HashSet<ProductItemResponse>();

  @BeforeEach
  public void init() {
    openMocks(this);
    this.predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(BRAND_CODE);
    ProductAttributeValueResponse productAttributeValueResponse =
        new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(BRAND_CODE);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(this.predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponseList.add(productAttributeValueResponse);
    BrandResponse brand = new BrandResponse();
    brand.setBrandCode(BRAND_CODE);
    brand.setBrandLogoPath(BRAND_LOGO_URL);
    this.brandResponse =
        new GdnRestSingleResponse<BrandResponse>(brand, REQUEST_ID);
    brandResponse.setSuccess(true);
    this.attribute = new AttributeResponse();
    attribute.setName("brand");
    this.productAttributeResponses = new ProductAttributeResponse();
    productAttributeResponses.setAttribute(attribute);
    productAttributeResponses.setProductAttributeValues(productAttributeValueResponseList);
    productAttributeResponses.setMarkForDelete(false);
    productAttributeResponsesList.add(this.productAttributeResponses);
    this.productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductAttributeResponses(productAttributeResponsesList);
    ProductItemResponse itemResponse = new ProductItemResponse();
    this.itemResponses.add(itemResponse);
    productDetailResponse.setProductItemResponses(this.itemResponses);
    this.masterDataProduct = new MasterDataProduct();
    masterDataProduct.setBrandLogoUrl(BRAND_LOGO_URL);
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsWhenAlwaysFetchInAllProductDataCacheTest() throws Exception {
    when(productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(productDetailResponse);
    when(productCategoryBaseOutbound.getBrandLogoUrl(BRAND_CODE)).thenReturn(BRAND_LOGO_URL);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataServiceImpl.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
            IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getBrandLogoUrl(BRAND_CODE);
    assertEquals(masterDataCacheVo.getBrandLogoUrl(), BRAND_LOGO_URL);
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsWhenAlwaysFetchInAllProductDataTest() throws Exception {
    when(productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(productDetailResponse);
    when(productCategoryBaseOutbound.getBrandLogoUrl(BRAND_CODE)).thenReturn(BRAND_LOGO_URL);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataServiceImpl.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME, PRODUCT_CODE,
            IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getBrandLogoUrl(BRAND_CODE);
    assertEquals(masterDataCacheVo.getBrandLogoUrl(), BRAND_LOGO_URL);
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsWhenAlwaysFetchInAllProductDataFalseTest() throws Exception {
    ReflectionTestUtils.setField(pcbMasterDataServiceImpl, "alwaysFetchInAllProductDataFromPCB", true);
    when(productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true)).thenReturn(productDetailResponse);
    when(productCategoryBaseOutbound.getBrandLogoUrl(BRAND_CODE)).thenReturn(BRAND_LOGO_URL);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataServiceImpl.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME, PRODUCT_CODE,
            IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true);
    verify(productCategoryBaseOutbound).getBrandLogoUrl(BRAND_CODE);
    assertEquals(masterDataCacheVo.getBrandLogoUrl(), BRAND_LOGO_URL);
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsWhenProductResponseNullTest() throws Exception {
    ReflectionTestUtils.setField(pcbMasterDataServiceImpl, "alwaysFetchInAllProductDataFromPCB", true);
    when(productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> pcbMasterDataServiceImpl.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME, PRODUCT_CODE,
              IN_ALL_PRODUCTS));
    } finally {
      verify(productCategoryBaseOutbound).getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME,
          PRODUCT_CODE, true);
    }
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsWhenBrandCodeEmptyTest() throws Exception {
    ReflectionTestUtils.setField(pcbMasterDataServiceImpl, "alwaysFetchInAllProductDataFromPCB", true);
    this.predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(null);
    productDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(null);
    when(productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true)).thenReturn(productDetailResponse);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataServiceImpl.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME, PRODUCT_CODE,
            IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true);
    assertEquals(masterDataCacheVo.getBrandLogoUrl(), EMPTY);
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsWhenProductAttributeEmptyTest() throws Exception {
    ReflectionTestUtils.setField(pcbMasterDataServiceImpl, "alwaysFetchInAllProductDataFromPCB", true);
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    when(productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true)).thenReturn(productDetailResponse);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataServiceImpl.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME, PRODUCT_CODE,
            IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true);
    assertEquals(masterDataCacheVo.getBrandLogoUrl(), EMPTY);
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsWhenBrandResponseFailureTest() throws Exception {
    ReflectionTestUtils.setField(pcbMasterDataServiceImpl, "alwaysFetchInAllProductDataFromPCB", true);
    when(productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true)).thenReturn(productDetailResponse);
    when(productCategoryBaseOutbound.getBrandLogoUrl(BRAND_CODE)).thenReturn(StringUtils.EMPTY);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataServiceImpl.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME, PRODUCT_CODE,
            IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true);
    verify(productCategoryBaseOutbound).getBrandLogoUrl(BRAND_CODE);
    assertEquals(masterDataCacheVo.getBrandLogoUrl(), EMPTY);
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsWhenBrandResponseNullTest() throws Exception {
    ReflectionTestUtils.setField(pcbMasterDataServiceImpl, "alwaysFetchInAllProductDataFromPCB", true);
    when(productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true)).thenReturn(productDetailResponse);
    when(productCategoryBaseOutbound.getBrandLogoUrl(BRAND_CODE)).thenReturn(BRAND_LOGO_URL);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataServiceImpl.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME, PRODUCT_CODE,
            IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true);
    verify(productCategoryBaseOutbound).getBrandLogoUrl(BRAND_CODE);
    assertEquals(masterDataCacheVo.getBrandLogoUrl(), BRAND_LOGO_URL);
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsWhenAttributeNameNotBrandTest() throws Exception {
    ReflectionTestUtils.setField(pcbMasterDataServiceImpl, "alwaysFetchInAllProductDataFromPCB", true);
    productAttributeResponsesList.get(0).getAttribute().setName("test");
    when(productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true)).thenReturn(productDetailResponse);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataServiceImpl.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME, PRODUCT_CODE,
            IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true);
    assertEquals(masterDataCacheVo.getBrandLogoUrl(), EMPTY);
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsWhenAttributeMfdTrueTest() throws Exception {
    ReflectionTestUtils.setField(pcbMasterDataServiceImpl, "alwaysFetchInAllProductDataFromPCB", true);
    productAttributeResponsesList.get(0).setMarkForDelete(true);
    when(productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true)).thenReturn(productDetailResponse);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataServiceImpl.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME, PRODUCT_CODE,
            IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true);
    assertEquals(masterDataCacheVo.getBrandLogoUrl(), EMPTY);
  }

  @Test
  public void getProductDetailByProductCodeForAllProductsWhenProductAttributeValueResponseEmptyTest() throws Exception {
    ReflectionTestUtils.setField(pcbMasterDataServiceImpl, "alwaysFetchInAllProductDataFromPCB", true);
    productAttributeResponsesList.get(0).setProductAttributeValues(new ArrayList<>());
    when(productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true)).thenReturn(productDetailResponse);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataServiceImpl.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME, PRODUCT_CODE,
            IN_ALL_PRODUCTS);
    verify(productCategoryBaseOutbound).getProductDetailByProductCodeForAllProducts(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true);
    assertEquals(masterDataCacheVo.getBrandLogoUrl(), EMPTY);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productCategoryBaseOutbound);
  }
}
