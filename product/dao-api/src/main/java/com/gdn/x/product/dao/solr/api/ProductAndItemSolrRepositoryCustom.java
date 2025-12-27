package com.gdn.x.product.dao.solr.api;

import java.util.List;
import java.util.Map;

import org.apache.solr.common.SolrInputDocument;
import org.joda.time.DateTime;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.ActiveProductsRequestVO;
import com.gdn.x.product.model.vo.BulkItemSummaryRequestVo;
import com.gdn.x.product.model.vo.ItemSummaryRequestVO;
import com.gdn.x.product.model.vo.OfficialStoreRequestVO;
import com.gdn.x.product.model.vo.SolrGroupResultVO;

public interface ProductAndItemSolrRepositoryCustom {
  /**
   * @param storeId
   * @return
   */
  Page<ProductAndItemSolr> getListOfActiveProductSkusSortedByProductCode(String storeId,
      Pageable pageable);

  /**
   * Query to solr using pagination. If fields in the itemFilter is null then that fields will be
   * ignored in the query criteria. The condition is using AND condition. All field is equal search
   * except itemName and itemSkuKeywords.
   *
   * @param storeId
   * @param itemFilter
   * @param orderBy
   * @param sortBy
   * @param pageRequest
   * @return null if all field in itemFilter is null or empty. Return empty page if no data found in
   *         SOLR
   */
  Page<ProductAndItemSolr> getProductAndItemsByFilter(String storeId, ItemSummaryRequestVO itemFilter, String orderBy,
      String sortBy, PageRequest pageRequest);

  /**
   * Query to solr using pagination. If fields in the itemFilter is null then that fields will be ignored in the query
   * criteria. The condition is using AND condition. All field is equal search except itemName and itemSkuKeywords.
   * Added the itemCodes in itemFilter.
   *
   * @param storeId     store identifier
   * @param itemFilter  filter request
   * @param pageRequest paging request details
   *
   * @param sortBy
   * @param orderBy
   * @return null if all field in itemFilter is null or empty. Return empty page if no data found in SOLR
   */
  Page<ProductAndItemSolr> getBulkProductAndItemsByFilter(String storeId, BulkItemSummaryRequestVo itemFilter,
      PageRequest pageRequest, String sortBy, String orderBy);

  /**
   * Query to solr to get products with mandatory merchantCode for official store
   *
   * @param storeId
   * @param officialStoreRequestVO
   * @param pageable
   * @return
   */
  SolrGroupResultVO<ProductAndItemSolr> getProductsForOfficialStore(String storeId,
      OfficialStoreRequestVO officialStoreRequestVO, Pageable pageable);

  /**
   * Query to solr l3 to get products with mandatory merchantCode for official store
   * @param storeId
   * @param officialStoreRequestVO
   * @param pageable
   * @return
   */
  SolrGroupResultVO<ProductSolr> getProductsByCategoryAndMerchantCodeL3(String storeId,
      OfficialStoreRequestVO officialStoreRequestVO, Pageable pageable);

  /**
   * Query to solr to get active products with mandatory merchantCode and category code for the merchant
   *
   * @param storeId
   * @param activeProductsRequestVO
   * @param pageable
   * @return
   */
  SolrGroupResultVO<ProductSolr> getActiveProductsListForMerchant(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageable);

  /**
   * @param storeId
   * @param catalogCode
   * @param categoryCode
   * @param searchEmptySalesOnly
   * @param page
   * @return
   */
  Page<ProductAndItemSolr> getProductsByMasterCatalog(String storeId, String catalogCode,
      String categoryCode, boolean searchEmptySalesOnly, Pageable page);

  /**
   * @param storeId
   * @param catalogCode
   * @param categoryCode
   * @param page
   * @return
   */
  Page<ProductAndItemSolr> getProductsBySalesCatalog(String storeId, String catalogCode,
      String categoryCode, Pageable page) throws Exception;

  /**
   * to get list items price by product skus
   *
   * @param storeId must not be blank
   * @param officialStoreRequestVO must not be null
   * @param productSkus must not be blank
   */
  Page<ProductAndItemSolr> getListItemsByProductSkus(String storeId, List<String> productSkus,
      OfficialStoreRequestVO officialStoreRequestVO, Pageable pageable);

  Page<ProductAndItemSolr> getProductsWithNullSalesCatalogAndMarkForDeleteFalse(String storeId,
    DateTime startDate, DateTime endDate, Pageable page);

  Page<ProductAndItemSolr>
  getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalse(
      String storeId, String merchantCode, List<String> categories, List<String> brand,
      String keyword, String productSku, PageRequest pageRequest);

  void deleteOfflineItemByItemIds(List<String> itemIds);

  /**
   * Get ItemSkus by merchantCode and createdDate
   * @param storeId
   * @param merchantCode
   * @param pageRequest
   * @return
   */
  Page<ProductAndItemSolr> getItemsByMerchantCode(String storeId, String merchantCode, Pageable pageRequest);

  /**
   * Index merchant promo discount to solr atomically
   *
   * @param solrInputDocument
   */
  void executeSolrDocumentAtomicUpdate(SolrInputDocument solrInputDocument) throws Exception;

  /**
   * Execute atomic update of solrInputDocuments
   *
   * @param solrInputDocuments
   * @throws Exception
   */
  void executeSolrDocumentsAtomicUpdate(List<SolrInputDocument> solrInputDocuments) throws Exception;

  /**
   * API to get the products based on the filter request
   * @param storeId
   * @param activeProductsRequestVO
   * @param pageRequest
   * @return
   */
  Page<ProductAndItemSolr> getProductsByMerchantCodeAndCategoryCodesAndStatus(
      String storeId, ActiveProductsRequestVO activeProductsRequestVO, Pageable pageRequest);


  /**
   *  API to get the products based on the filter request V2
   * @param storeId
   * @param activeProductsRequestVO
   * @param pageRequest
   * @return
   */
  Page<ProductAndItemSolr> getProductsByMerchantCodeAndCategoryCodesAndStatusV2(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageRequest);

  /**
   * Get suspended ItemSkus based on the filter request
   * @param storeId
   * @param activeProductsRequestVO
   * @param pageRequest
   * @return
   */
  Page<ProductAndItemSolr> getItemsByMerchantCodeAndCategoryCodes(
      String storeId, ActiveProductsRequestVO activeProductsRequestVO, Pageable pageRequest) throws Exception;

  /**
   * Get ItemSkus based on the productSku
   * @param storeId
   * @param productSkus
   * @return
   */
  Map<String, Long> getItemsByProductSkus(String storeId, List<String> productSkus);

  /**
   * Query to solr using pagination. If fields in the itemFilter is null then that fields will be
   * ignored in the query criteria. This query performs boosting on a list of boostProductSkus if present.
   *
   * @param storeId
   * @param itemSummaryRequestVO
   * @param orderBy
   * @param sortBy
   * @param pageRequest
   * @return
   */
  Page<ProductAndItemSolr> getPromoProductAndItemsByFilter(String storeId, ItemSummaryRequestVO itemSummaryRequestVO,
      String orderBy, String sortBy, PageRequest pageRequest);
}