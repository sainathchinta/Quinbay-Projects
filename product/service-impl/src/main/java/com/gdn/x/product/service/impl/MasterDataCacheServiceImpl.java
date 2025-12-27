package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.MasterDataCacheVo;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.MarkForDeleteHelperService;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PCBMasterDataService;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.google.common.base.Stopwatch;

@Service
public class MasterDataCacheServiceImpl implements MasterDataCacheService {
  private static final String PRODUCT_ITEM_NOT_FOUND_WITH_ITEM_CODE =
      "product item not found with item code : ";

  private static final String ITEM_CODE_MUST_NOT_BE_BLANK = "itemCode must not be blank";

  private static final String PRODUCT_CODE_MUST_NOT_BE_BLANK = "productCode must not be blank";

  private static final Logger LOGGER = LoggerFactory.getLogger(MasterDataCacheServiceImpl.class);

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private MarkForDeleteHelperService markForDeleteHelperService;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  private PCBMasterDataService pcbMasterDataService;
  
  @Autowired
  @Qualifier("redisTemplateMasterDataCombination")
  private RedisTemplate<String, Object> masterDataRedisTemplate;

  @Autowired
  @Qualifier("lettuceCategoryCombination")
  private LettuceConnectionFactory lettuceCategoryConnectionFactory;

  @Override
  public void evictAllCategoryTrees() {
    cacheEvictHelperService.flushRedisDBByJedisConnectionFactory(lettuceCategoryConnectionFactory);
  }

  @Override
  public void evictCachesMasterDataItem(List<String> itemCodes) {
    if (!CollectionUtils.isEmpty(itemCodes)) {
      for (String itemCode : itemCodes) {
        String key = CacheNames.GET_MASTER_DATA_ITEM + ":" + itemCode;
        this.masterDataRedisTemplate.delete(key);
      }
    }
  }

  @Override
  @Cacheable(cacheManager = Constants.MASTER_DATA_CACHE_MANAGER, value = {
      CacheNames.GET_MASTER_DATA_FOR_TRANSACTION}, key = "#itemCode", unless = "#result == null")
  public ProductMasterDataResponse getProductMasterDataForTransaction(String itemCode) throws Exception {
    return this.productCategoryBaseOutbound.getMasterDataForTransaction(itemCode);
  }

  @Override
  @CacheEvict(cacheManager = Constants.MASTER_DATA_CACHE_MANAGER, value = CacheNames.GET_MASTER_DATA_FOR_TRANSACTION,
    key = "#itemCode")
  public void evictCacheMasterDataForTransaction(String itemCode) {
  }

  @Override
  @CacheEvict(cacheManager = Constants.CATEGORY_CACHE_MANAGER, value = {
      CacheNames.FIND_CATEGORY_CODES_BY_ATTRIBUTE_CODE}, key = "#storeId + '-' + #attributeCode")
  public void evictCategoryCodesByAttributeCodeCache(String storeId, String attributeCode) {
    //Evict cache on category update with key as AttributeCode
  }

  @Override
  public void evictCachesMasterDataProduct(List<String> productCodes) {
    if (!CollectionUtils.isEmpty(productCodes)) {
      for (String productCode : productCodes) {
        String key = CacheNames.GET_PRODUCT_DATA_WITH_BRAND_LOGO + ":" + productCode;
        this.masterDataRedisTemplate.delete(key);
      }
    }
  }
  
  @Override
  @CacheEvict(cacheManager = Constants.MASTER_DATA_CACHE_MANAGER,
      value = {CacheNames.GET_PRODUCT_DATA_WITH_BRAND_LOGO},
      key = "#productCode")
  public ProductDomainEventModel evictMasterDataProduct(String productCode,
      ProductDomainEventModel productDomainEventModel) {
    return productDomainEventModel;
  }

  @Override
  @CacheEvict(cacheManager = Constants.MASTER_DATA_CACHE_MANAGER,
      value = {CacheNames.GET_PRODUCT_DATA_WITH_BRAND_LOGO},
      key = "#productCode")
  public ProductDomainEventModel evictMasterDataProductWithoutSolrIndex(String productCode,
      ProductDomainEventModel productDomainEventModel) {
    return productDomainEventModel;
  }

  @Override
  @CacheEvict(cacheManager = Constants.MASTER_DATA_CACHE_MANAGER, value = {
      CacheNames.GET_MASTER_DATA_ITEM}, key = "#itemCode")
  public void evictMasterDataItem(String itemCode,
      ProductItemDomainEventModel productItemDomainEventModel) {
    // used for cache eviction purpose only
  }

  @Override
  @Cacheable(cacheManager = Constants.MASTER_DATA_CACHE_MANAGER, value = {
      CacheNames.GET_MASTER_DATA_ITEM}, key = "#itemCode", unless = "#result == null")
  public MasterDataItem getMasterDataItem(String requestId, String username, String itemCode) {
    return getMasterDataItemWithoutCache(requestId, username, itemCode);
  }

  public MasterDataItem getMasterDataItemWithoutCache(String requestId, String username, String itemCode) {
    checkArgument(StringUtils.isNotBlank(itemCode),
        MasterDataCacheServiceImpl.ITEM_CODE_MUST_NOT_BE_BLANK);
    Stopwatch stopwatch = Stopwatch.createStarted();
    long elapsed = stopwatch.elapsed(TimeUnit.NANOSECONDS);
    ProductItemDetailResponse productItem = this.productCategoryBaseOutbound
        .getProductItemDetailByItemCode(requestId, username, itemCode);
    MasterDataCacheServiceImpl.LOGGER.info("getMasterDataItem {} : {}", itemCode,
        stopwatch.elapsed(TimeUnit.NANOSECONDS) - elapsed);
    checkArgument(productItem != null,
        MasterDataCacheServiceImpl.PRODUCT_ITEM_NOT_FOUND_WITH_ITEM_CODE + itemCode);
    productItem = this.markForDeleteHelperService.removeAllMarkForDelete(productItem);
    return this.objectConverterService.convertToMasterDataItem(productItem);
  }

  @Override
  public MasterDataProductAndItemsVO getMasterDataProductAndItems(String username, String requestId, String productCode,
      boolean inAllProducts) throws Exception {
    checkArgument(StringUtils.isNotBlank(productCode), MasterDataCacheServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataService.getProductDetailByProductCodeForAllProductsCached(requestId, username, productCode,
            inAllProducts);
    return toMasterDataProductAndItemVo(productCode, inAllProducts, masterDataCacheVo);
  }

  @Override
  public MasterDataProductAndItemsVO getMasterDataProductAndItemsWithoutCache(String username, String requestId,
      String productCode, boolean inAllProducts) throws Exception {
    checkArgument(StringUtils.isNotBlank(productCode), MasterDataCacheServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataService.getProductDetailByProductCodeForAllProductsWithoutCache(requestId, username, productCode,
            inAllProducts);
    return toMasterDataProductAndItemVo(productCode, inAllProducts, masterDataCacheVo);
  }

  private MasterDataProductAndItemsVO toMasterDataProductAndItemVo(String productCode, boolean inAllProducts,
      MasterDataCacheVo masterDataCacheVo) {
    ProductDetailResponse productResponse = masterDataCacheVo.getProductResponse();
    String brandLogoUrl = masterDataCacheVo.getBrandLogoUrl();
    if (!inAllProducts) {
      productResponse = this.markForDeleteHelperService.removeAllMarkForDelete(productResponse);
    }
    MasterDataProduct masterDataProduct = this.objectConverterService.convertToMasterDataProduct(productResponse);
    masterDataProduct.setBrandLogoUrl(brandLogoUrl);
    Map<String, MasterDataItem> masterDataItems =
        this.objectConverterService.convertToMasterDataItems(productResponse.getProductItemResponses(), productCode);
    Map<String, String> categoryCodeAndCategoryName = new HashMap<>();
    if (CollectionUtils.isNotEmpty(productResponse.getProductCategoryResponses())) {
      for (ProductCategoryResponse productCategoryResponse : productResponse.getProductCategoryResponses()) {
        categoryCodeAndCategoryName.put(productCategoryResponse.getCategory().getCategoryCode(),
            productCategoryResponse.getCategory().getName());
      }
    }
    return new MasterDataProductAndItemsVO(masterDataProduct, masterDataItems, categoryCodeAndCategoryName);
  }

  @CacheEvict(cacheManager = Constants.MASTER_DATA_CACHE_MANAGER, value = {
    CacheNames.GET_PRODUCT_DATA_WITH_BRAND_LOGO}, key = "#productCode")
  @Override
  public void evictMasterDataProduct(String productCode) {
    // used for cache eviction OF master Data
  }
}
