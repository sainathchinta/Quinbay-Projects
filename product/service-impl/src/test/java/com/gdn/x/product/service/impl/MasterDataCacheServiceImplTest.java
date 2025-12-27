package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.collections.MapUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.slf4j.MDC;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.vo.MasterDataCacheVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.outbound.api.feign.PCBFeign;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.MarkForDeleteHelperService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PCBMasterDataService;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;

public class MasterDataCacheServiceImplTest {

  private static final String ITEM_CODE = "item-code";

  private static final String USERNAME = "username";

  private static final String STORE_ID = "storeId";

  private static final String REQUEST_ID = "request-id";

  private static final String PRODUCT_CODE = "product-code";

  private static final String BRAND_LOGO_URL = "brandLogoUrl";

  private static final String CATEGORY_CODE = "categoryCode";
  private static final String ATTRIBUTE_CODE = "attributeCode";

  private static final String CATEGORY_NAME = "categoryName";

  private static final String EMPTY = "";

  private static final boolean IN_ALL_PRODUCTS = false;

  @InjectMocks
  private MasterDataCacheServiceImpl masterDataHelperServiceImpl;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Mock
  private MarkForDeleteHelperService markForDeleteHelperService;

  @Mock
  private PCBFeign brandClient;

  @Mock
  private PCBMasterDataService pcbMasterDataService;

  @Mock
  private RedisTemplate<String, Object> stringRedisTemplate;

  @Mock
  private LettuceConnectionFactory lettuceCategoryConnectionFactory;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  private ProductDetailResponse productDetailResponse;

  private GdnRestSingleResponse<BrandResponse> brandResponse;

  private AttributeResponse attribute;

  private MasterDataProduct masterDataProduct;

  private ProductAttributeResponse productAttributeResponses;

  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();

  private HashSet<ProductItemResponse> itemResponses = new HashSet<ProductItemResponse>();

  private List<ProductAttributeValueResponse> productAttributeValueResponseList =
      new ArrayList<ProductAttributeValueResponse>();

  private List<ProductAttributeResponse> productAttributeResponsesList = new ArrayList<>();

  @Test
  public void evictAllCategoryTreesTest() {
    this.masterDataHelperServiceImpl.evictAllCategoryTrees();
    Mockito.verify(cacheEvictHelperService).flushRedisDBByJedisConnectionFactory(lettuceCategoryConnectionFactory);
  }

  @Test
  public void evictAndReindexMasterDataProductTest() {
    ProductDomainEventModel productDomainEventParam = new ProductDomainEventModel();
    ProductDomainEventModel productDomainEventModel =
        this.masterDataHelperServiceImpl.evictMasterDataProduct(MasterDataCacheServiceImplTest.PRODUCT_CODE,
            productDomainEventParam);
    assertEquals(productDomainEventParam, productDomainEventModel);
  }

  @Test
  public void evictAndReindexMasterDataProductWithoutSolrIndexTest() {
    ProductDomainEventModel productDomainEventParam = new ProductDomainEventModel();
    ProductDomainEventModel productDomainEventModel =
        this.masterDataHelperServiceImpl.evictMasterDataProductWithoutSolrIndex(
            MasterDataCacheServiceImplTest.PRODUCT_CODE, productDomainEventParam);
    assertEquals(productDomainEventParam, productDomainEventModel);
  }

  @Test
  public void evictMasterDataItemTest() {
    this.masterDataHelperServiceImpl.evictMasterDataItem(MasterDataCacheServiceImplTest.ITEM_CODE,
        new ProductItemDomainEventModel());
  }

  @Test
  public void evictCachesMasterDataItemTest() {
    List<String> itemCodes = new ArrayList<>();
    itemCodes.add(ITEM_CODE);
    when(this.stringRedisTemplate.hasKey(anyString())).thenReturn(true);
    this.masterDataHelperServiceImpl.evictCachesMasterDataItem(itemCodes);
    verify(this.stringRedisTemplate, times(1)).delete(anyString());
  }

  @Test
  public void evictCachesMasterDataProductTest() {
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
    when(this.stringRedisTemplate.hasKey(anyString())).thenReturn(true);
    this.masterDataHelperServiceImpl.evictCachesMasterDataProduct(productCodes);
    verify(this.stringRedisTemplate).delete(anyString());
  }

  @Test
  public void evictCachesMasterDataItemEmptyTest() {
    List<String> itemCodes = null;
    this.masterDataHelperServiceImpl.evictCachesMasterDataItem(itemCodes);
  }

  @Test
  public void evictCachesMasterDataProductEmptyTest() {
    List<String> productCodes = null;
    this.masterDataHelperServiceImpl.evictCachesMasterDataProduct(productCodes);
  }

  @Test
  public void evictCachesMasterDataItemEmptyKeyTest() {
    List<String> itemCodes = new ArrayList<>();
    itemCodes.add(ITEM_CODE);
    this.masterDataHelperServiceImpl.evictCachesMasterDataItem(itemCodes);
    verify(this.stringRedisTemplate, times(1)).delete(anyString());
  }

  @Test
  public void evictCachesMasterDataProductEmptyKeyTest() {
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
    this.masterDataHelperServiceImpl.evictCachesMasterDataProduct(productCodes);
    verify(this.stringRedisTemplate).delete(anyString());
  }

  @Test
  public void getMasterDataItemSuccess() {
    ProductItemDetailResponse productItemResponse = new ProductItemDetailResponse();
    when(this.productCategoryBaseOutbound.getProductItemDetailByItemCode(MasterDataCacheServiceImplTest.REQUEST_ID,
        MasterDataCacheServiceImplTest.USERNAME, MasterDataCacheServiceImplTest.ITEM_CODE)).thenReturn(
        productItemResponse);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productItemResponse)).thenReturn(productItemResponse);
    this.masterDataHelperServiceImpl.getMasterDataItem(MasterDataCacheServiceImplTest.REQUEST_ID,
        MasterDataCacheServiceImplTest.USERNAME, MasterDataCacheServiceImplTest.ITEM_CODE);
    verify(this.productCategoryBaseOutbound).getProductItemDetailByItemCode(MasterDataCacheServiceImplTest.REQUEST_ID,
        MasterDataCacheServiceImplTest.USERNAME, MasterDataCacheServiceImplTest.ITEM_CODE);
    verify(this.objectConverterService).convertToMasterDataItem(productItemResponse);
  }


  @Test
  public void getMasterDataItemWithEmptySetOfItemCodes() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.masterDataHelperServiceImpl.getMasterDataItem(MasterDataCacheServiceImplTest.REQUEST_ID,
        MasterDataCacheServiceImplTest.USERNAME, ""));
  }

  @Test
  public void getMasterDataItemWithNullResponseFromPCB() {
    ProductItemDetailResponse productItemResponse = new ProductItemDetailResponse();
    when(this.productCategoryBaseOutbound.getProductItemDetailByItemCode(MasterDataCacheServiceImplTest.REQUEST_ID,
        MasterDataCacheServiceImplTest.USERNAME, MasterDataCacheServiceImplTest.ITEM_CODE)).thenReturn(null);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productItemResponse)).thenReturn(productItemResponse);
    try {
      this.masterDataHelperServiceImpl.getMasterDataItem(MasterDataCacheServiceImplTest.REQUEST_ID,
          MasterDataCacheServiceImplTest.USERNAME, MasterDataCacheServiceImplTest.ITEM_CODE);
    } catch (Exception e) {
      verify(this.productCategoryBaseOutbound).getProductItemDetailByItemCode(MasterDataCacheServiceImplTest.REQUEST_ID,
          MasterDataCacheServiceImplTest.USERNAME, MasterDataCacheServiceImplTest.ITEM_CODE);
      assertEquals(e instanceof ApplicationRuntimeException, true);
    }
  }

  @Test
  public void getMasterDataItemWithNullSetOfItemCodes() {
    Assertions.assertThrows(Exception.class, () ->  this.masterDataHelperServiceImpl.getMasterDataItem(MasterDataCacheServiceImplTest.REQUEST_ID,
        MasterDataCacheServiceImplTest.USERNAME, null));
  }

  @Test
  public void getMasterDataProductSuccess() throws Exception {
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, BRAND_LOGO_URL);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(
        this.masterDataProduct);
    this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
        MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
  }

  @Test
  public void getMasterDataProductWithInAllProductsTrueSuccess() throws Exception {
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, EMPTY);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        true)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(
        this.masterDataProduct);
    this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
        MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, true);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, true);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
  }

  @Test
  public void getMasterDataProductWhenPrdAttrIsNull() throws Exception {
    productDetailResponse.setProductAttributeResponses(null);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(
        this.masterDataProduct);
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, EMPTY);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
        MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
  }

  @Test
  public void getMasterDataProductWithNullProductCode() throws Exception {
    Assertions.assertThrows(Exception.class, () ->  this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
        MasterDataCacheServiceImplTest.REQUEST_ID, null, IN_ALL_PRODUCTS));
  }

  @Test
  public void getMasterDataProductAndItems_SuccessIsTrueBrandIsNotNull_BrandLogoUrl() throws Exception {
    this.brandResponse.setSuccess(true);
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, BRAND_LOGO_URL);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(
        this.masterDataProduct);
    when(this.brandClient.getBrandResponseByBrandCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), MasterDataCacheServiceImplTest.BRAND_LOGO_URL)).thenReturn(
        this.brandResponse);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
            MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    assertEquals(masterDataProductAndItemsVO.getMasterDataProduct().getBrandLogoUrl(), BRAND_LOGO_URL);
  }

  @Test
  public void getMasterDataProductAndItems_SuccessIsTrueBrandIsNull_BrandLogoUrlIsNull() throws Exception {
    BrandResponse brand = null;
    this.brandResponse = new GdnRestSingleResponse<BrandResponse>(brand, MasterDataCacheServiceImplTest.REQUEST_ID);
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, EMPTY);
    this.masterDataProduct.setBrandLogoUrl(null);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(masterDataProduct);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
            MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    assertEquals(masterDataProductAndItemsVO.getMasterDataProduct().getBrandLogoUrl(),
        MasterDataCacheServiceImplTest.EMPTY);
  }

  @Test
  public void getMasterDataProductAndItems_SuccessIsFalseBrandIsNull_BrandLogoUrlIsNull() throws Exception {
    BrandResponse brand = null;
    this.brandResponse = new GdnRestSingleResponse<BrandResponse>(brand, MasterDataCacheServiceImplTest.REQUEST_ID);
    this.brandResponse.setSuccess(false);
    this.masterDataProduct.setBrandLogoUrl(null);
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, EMPTY);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(masterDataProduct);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
            MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    assertEquals(masterDataProductAndItemsVO.getMasterDataProduct().getBrandLogoUrl(),
        MasterDataCacheServiceImplTest.EMPTY);
  }

  @Test
  public void getMasterDataProductAndItems_ProductAttributeValueResponseListIsEmpty_BrandLogUrlIsNull()
      throws Exception {
    List<ProductAttributeValueResponse> productAttributeValueResponseEmptyList =
        new ArrayList<ProductAttributeValueResponse>();
    productAttributeResponses.setProductAttributeValues(productAttributeValueResponseEmptyList);
    BrandResponse brand = null;
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, EMPTY);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    this.brandResponse = new GdnRestSingleResponse<BrandResponse>(brand, MasterDataCacheServiceImplTest.REQUEST_ID);
    this.brandResponse.setSuccess(false);
    masterDataProduct.setBrandLogoUrl(null);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(masterDataProduct);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
            MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    assertEquals(masterDataProductAndItemsVO.getMasterDataProduct().getBrandLogoUrl(),
        MasterDataCacheServiceImplTest.EMPTY);
  }

  @Test
  public void getMasterDataProductAndItems_AttributeNameIsNotBrand_BrandLogoUrlIsNull() throws Exception {
    this.attribute.setName("name");
    BrandResponse brand = null;
    this.brandResponse = new GdnRestSingleResponse<BrandResponse>(brand, MasterDataCacheServiceImplTest.REQUEST_ID);
    this.brandResponse.setSuccess(false);
    masterDataProduct.setBrandLogoUrl(null);
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, EMPTY);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(
        this.masterDataProduct);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
            MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
    assertEquals(masterDataProductAndItemsVO.getMasterDataProduct().getBrandLogoUrl(),
        MasterDataCacheServiceImplTest.EMPTY);
  }

  @Test
  public void getMasterDataProductAndItems_MarkForDeleteIsTrue_BrandLogoUrlIsNull() throws Exception {
    this.productAttributeResponses.setMarkForDelete(true);
    BrandResponse brand = null;
    this.brandResponse = new GdnRestSingleResponse<BrandResponse>(brand, MasterDataCacheServiceImplTest.REQUEST_ID);
    this.brandResponse.setSuccess(false);
    masterDataProduct.setBrandLogoUrl(null);
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, EMPTY);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(
        this.masterDataProduct);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
            MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    assertEquals(masterDataProductAndItemsVO.getMasterDataProduct().getBrandLogoUrl(),
        MasterDataCacheServiceImplTest.EMPTY);
  }

  @Test
  public void getMasterDataProductAndItems_MarkForDeleteIsTrueAndAttributeNameIsNotBrand_BrandLogoUrlIsNull()
      throws Exception {
    this.productAttributeResponses.setMarkForDelete(true);
    this.attribute.setName("name");

    BrandResponse brand = null;
    this.brandResponse = new GdnRestSingleResponse<BrandResponse>(brand, MasterDataCacheServiceImplTest.REQUEST_ID);
    this.brandResponse.setSuccess(false);
    masterDataProduct.setBrandLogoUrl(null);
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, EMPTY);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(
        this.masterDataProduct);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
            MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    assertEquals(masterDataProductAndItemsVO.getMasterDataProduct().getBrandLogoUrl(),
        MasterDataCacheServiceImplTest.EMPTY);
  }

  @Test
  public void getMasterDataProductAndItems_ListSizeIsMoreThanTwo_BrandLogoIsNull() throws Exception {
    ProductAttributeValueResponse productAttributeValueResponse2 = new ProductAttributeValueResponse();
    productAttributeValueResponse2.setPredefinedAllowedAttributeValue(this.predefinedAllowedAttributeValueResponse);
    productAttributeValueResponseList.add(productAttributeValueResponse2);
    BrandResponse brand = new BrandResponse();
    brand.setBrandCode("");
    brand.setBrandLogoPath(null);
    this.brandResponse = new GdnRestSingleResponse<BrandResponse>(brand, MasterDataCacheServiceImplTest.REQUEST_ID);
    brandResponse.setSuccess(true);

    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, EMPTY);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(
        this.masterDataProduct);
    when(this.brandClient.getBrandResponseByBrandCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), MasterDataCacheServiceImplTest.EMPTY)).thenReturn(
        brandResponse);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
            MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    assertEquals(masterDataProductAndItemsVO.getMasterDataProduct().getBrandLogoUrl(),
        MasterDataCacheServiceImplTest.EMPTY);
  }

  @Test
  public void getMasterDataProductAndItems_ListSizeIsOne_BrandLogoIsNotNull() throws Exception {
    when(this.productCategoryBaseOutbound.getProductDetailByProductCode(MasterDataCacheServiceImplTest.REQUEST_ID,
        MasterDataCacheServiceImplTest.USERNAME, MasterDataCacheServiceImplTest.PRODUCT_CODE)).thenReturn(
        this.productDetailResponse);
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, BRAND_LOGO_URL);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME, PRODUCT_CODE,
        IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(
        this.masterDataProduct);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        this.masterDataHelperServiceImpl.getMasterDataProductAndItems(MasterDataCacheServiceImplTest.USERNAME,
            MasterDataCacheServiceImplTest.REQUEST_ID, MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsCached(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
    assertEquals(masterDataProductAndItemsVO.getMasterDataProduct().getBrandLogoUrl(), BRAND_LOGO_URL);
  }

  @Test
  public void getProductMasterDataForTransactionTest() throws Exception {
    Mockito.when(this.productCategoryBaseOutbound.getMasterDataForTransaction(ITEM_CODE))
        .thenReturn(new ProductMasterDataResponse());
    masterDataHelperServiceImpl.getProductMasterDataForTransaction(ITEM_CODE);
    Mockito.verify(this.productCategoryBaseOutbound).getMasterDataForTransaction(ITEM_CODE);
  }

  @Test
  public void evictCacheMasterDataForTransactionTest() {
    masterDataHelperServiceImpl.evictCacheMasterDataForTransaction(ITEM_CODE);
  }

  @BeforeEach
  public void init() {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, MasterDataCacheServiceImplTest.USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, MasterDataCacheServiceImplTest.REQUEST_ID);
    openMocks(this);
    this.predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(BRAND_LOGO_URL);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(this.predefinedAllowedAttributeValueResponse);
    productAttributeValueResponseList.add(productAttributeValueResponse);
    BrandResponse brand = new BrandResponse();
    brand.setBrandCode("brandCode");
    brand.setBrandLogoPath(BRAND_LOGO_URL);
    this.brandResponse = new GdnRestSingleResponse<BrandResponse>(brand, MasterDataCacheServiceImplTest.REQUEST_ID);
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
  public void getMasterDataProductWithoutCacheSuccess() throws Exception {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponse.setName(CATEGORY_NAME);
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse(categoryResponse, STORE_ID);
    productDetailResponse.setProductCategoryResponses(Arrays.asList(productCategoryResponse));
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, BRAND_LOGO_URL);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(
        this.masterDataProduct);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        this.masterDataHelperServiceImpl.getMasterDataProductAndItemsWithoutCache(
            MasterDataCacheServiceImplTest.USERNAME, MasterDataCacheServiceImplTest.REQUEST_ID,
            MasterDataCacheServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME,
        PRODUCT_CODE, IN_ALL_PRODUCTS);
   Assertions.assertTrue(MapUtils.isNotEmpty(masterDataProductAndItemsVO.getCategoryCodeAndCategoryNameMap()));
   Assertions.assertEquals(CATEGORY_NAME,
        masterDataProductAndItemsVO.getCategoryCodeAndCategoryNameMap().get(CATEGORY_CODE));
  }

  @Test
  public void getMasterDataProductWithoutCacheSuccess_InAllProductFalse() throws Exception {
    MasterDataCacheVo masterDataCacheVo = new MasterDataCacheVo(productDetailResponse, BRAND_LOGO_URL);
    when(this.pcbMasterDataService.getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME,
        PRODUCT_CODE, true)).thenReturn(masterDataCacheVo);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    when(this.objectConverterService.convertToMasterDataProduct(productDetailResponse)).thenReturn(
        this.masterDataProduct);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        this.masterDataHelperServiceImpl.getMasterDataProductAndItemsWithoutCache(
            MasterDataCacheServiceImplTest.USERNAME, MasterDataCacheServiceImplTest.REQUEST_ID,
            MasterDataCacheServiceImplTest.PRODUCT_CODE, true);
    verify(this.objectConverterService).convertToMasterDataProduct(productDetailResponse);
    verify(this.objectConverterService).convertToMasterDataItems(itemResponses,
        MasterDataCacheServiceImplTest.PRODUCT_CODE);
    verify(this.pcbMasterDataService).getProductDetailByProductCodeForAllProductsWithoutCache(REQUEST_ID, USERNAME,
        PRODUCT_CODE, true);
   Assertions.assertTrue(MapUtils.isEmpty(masterDataProductAndItemsVO.getCategoryCodeAndCategoryNameMap()));
  }

  @Test
  public void getMasterDataProductWithoutCache_NullProductCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.masterDataHelperServiceImpl.getMasterDataProductAndItemsWithoutCache(MasterDataCacheServiceImplTest.USERNAME,
        MasterDataCacheServiceImplTest.REQUEST_ID, null, IN_ALL_PRODUCTS));
  }

  @Test
  public void evictCategoryCodesByAttributeCodeCacheTest(){
    masterDataHelperServiceImpl.evictCategoryCodesByAttributeCodeCache(Constants.DEFAULT_STORE_ID,
        ATTRIBUTE_CODE);
    Assertions.assertTrue(true);
  }

  @Test
  public void evictMasterDataCacheTest() {
    masterDataHelperServiceImpl.evictMasterDataProduct(PRODUCT_CODE);
    Assertions.assertTrue(true);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.productCategoryBaseOutbound);
    verifyNoMoreInteractions(this.stringRedisTemplate);
    verifyNoMoreInteractions(this.cacheEvictHelperService);
    verifyNoMoreInteractions(this.lettuceCategoryConnectionFactory);
  }

}
