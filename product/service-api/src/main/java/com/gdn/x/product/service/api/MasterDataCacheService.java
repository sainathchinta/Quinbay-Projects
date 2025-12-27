package com.gdn.x.product.service.api;

import java.util.List;

import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;

public interface MasterDataCacheService {
  /**
   * Evict all cache containing categoryParent key
   */
  void evictAllCategoryTrees();

  /**
   * @param productDomainEventModel
   */
  ProductDomainEventModel evictMasterDataProduct(String productCode,
      ProductDomainEventModel productDomainEventModel);


  /**
   * Evict master data cache without solr index
   * @param productCode
   * @param productDomainEventModel
   * @return
   */
  ProductDomainEventModel evictMasterDataProductWithoutSolrIndex(String productCode,
      ProductDomainEventModel productDomainEventModel);


  /**
   * @param productItemDomainEventModel
   */
  void evictMasterDataItem(String itemCode,
      ProductItemDomainEventModel productItemDomainEventModel);

  /**
   * @param requestId
   * @param username
   * @param itemCode
   * @return
   */
  MasterDataItem getMasterDataItem(String requestId, String username, String itemCode);

  /**
   * @param requestId
   * @param username
   * @param itemCode
   * @return
   */
  MasterDataItem getMasterDataItemWithoutCache(String requestId, String username, String itemCode);

  /**
   * MasterDataItems will returned without the masterDataItemAttribute data
   *
   * @param username
   * @param requestId
   * @param productCode
   * @param inAllProducts
   * @return masterDataProduct with its masterDataItems
   * @throws Exception
   */
  MasterDataProductAndItemsVO getMasterDataProductAndItems(String username, String requestId,
      String productCode, boolean inAllProducts) throws Exception;


  /**
   * MasterDataItems will returned without the masterDataItemAttribute data
   *
   * @param username
   * @param requestId
   * @param productCode
   * @param inAllProducts
   * @return masterDataProduct with its masterDataItems
   * @throws Exception
   */
  MasterDataProductAndItemsVO getMasterDataProductAndItemsWithoutCache(String username, String requestId,
      String productCode, boolean inAllProducts) throws Exception;

  /**
   * 
   * @param productCodes
   */
  void evictCachesMasterDataProduct(List<String> productCodes) throws Exception;

  /**
   * 
   * @param itemCodes
   */
  void evictCachesMasterDataItem(List<String> itemCodes) throws Exception;

  /**
   * Get master data for transacton by sku code
   *
   * @param itemCode
   * @return
   * @throws Exception
   */
  ProductMasterDataResponse getProductMasterDataForTransaction(String itemCode) throws Exception;

  /**
   * Get master data for transaction
   *
   * @param itemCode
   */
  void evictCacheMasterDataForTransaction(String itemCode);

  /**
   * Evict Category Codes By Attribute Code Cache on category Update
   * @param attributeCode
   */
  void evictCategoryCodesByAttributeCodeCache(String storeId, String attributeCode);

  /**
   * Evict Product Master Data Cache
   * @param productCode product code
   */
  void evictMasterDataProduct(String productCode);
}
