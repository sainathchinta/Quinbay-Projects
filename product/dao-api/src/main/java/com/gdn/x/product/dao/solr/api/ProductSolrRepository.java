package com.gdn.x.product.dao.solr.api;

import java.util.List;
import java.util.Set;

import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import org.apache.solr.common.SolrInputDocument;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.HalalDashboardFilterRequestVo;
import com.gdn.x.product.model.vo.HalalDashboardProductsResponseVo;
import com.gdn.x.product.model.vo.ProductCenterSummaryRequest;
import com.gdn.x.product.model.vo.ProductCountResponseVo;
import com.gdn.x.product.model.vo.ProductSkuSummaryRequestVo;
import com.gdn.x.product.model.vo.ProductSummaryRequestV2Vo;
import com.gdn.x.product.model.vo.ProductSummaryRequestVo;
import com.gdn.x.product.model.vo.ProductSummaryResponseV2Vo;
import com.gdn.x.product.model.vo.ReelProductListingRequestVo;

public interface ProductSolrRepository {

  /**
   *
   * @param productSku
   * @return
   */
  ProductSolr findOneByProductSkuAndMarkForDeleteFalse(String productSku);

  /**
   *
   * @param productCode
   * @param markForDelete
   * @return
   */
  List<ProductSolr> findByProductCode(String productCode, Boolean markForDelete);

  /**
   * Fetch product details with productCode and merchantCode from Solr
   * @param merchantCode
   * @param productSku
   * @return
   */
  ProductSolr findByProductSku(String merchantCode, String productSku);

  /**
   *
   * @param productSkuList
   */
  void deleteSolrDocumentsByListOfProductSku(Set<String> productSkuList);

  /**
   *
   * @param storeId
   * @param productCenterSummaryRequest
   * @param pageRequest
   * @return
   */
  Page<ProductSolr> getProductCenterSummary(String storeId,
      ProductCenterSummaryRequest productCenterSummaryRequest, PageRequest pageRequest) throws Exception;

  /**
   *
   * @param storeId
   * @param masterCategoryCodes
   * @return
   * @throws Exception
   */
  List<ProductSolr> getUnmappedProductSKusByCategoryCodes(String storeId, List<String> masterCategoryCodes) throws Exception;

  /**
   * Fetch product solr documents by request
   *
   * @param storeId
   * @param productSkuSummaryRequestVo
   * @param businessPartnerCode
   * @return
   */
  Page<ProductSolr> getProductSkuSummary(String storeId,
      ProductSkuSummaryRequestVo productSkuSummaryRequestVo, String businessPartnerCode, int page, int size);


  /**
   * Update L3 stock status
   *
   * @param productSku
   * @param status
   * @param merchantCode
   */
  void updateStockStatus(String productSku, boolean status, String merchantCode);

  /**
   *
   * @param items
   * @param isPromoUpdateOnly
   * @param <T> T can be either Item or ItemPickupPoint
   */
  <T> void updatePromoOrWholesaleItemSkus(List<T> items, boolean isPromoUpdateOnly);

  /**
   *
   * @param productCode
   * @return
   */
  Long countByProductCode(String productCode);


  /**
   * fetching products for Halal Dashboard from solr
   *
   * @param storeId
   * @param page
   * @param size
   * @param halalDashboardFilterRequestVo
   * @return
   */
  Page<HalalDashboardProductsResponseVo> getHalalDashboardProductsResponse(String storeId, int page, int size,
      HalalDashboardFilterRequestVo halalDashboardFilterRequestVo);


  /**
   * Get L3 summary by ProductSummaryRequest
   *
   *
   * @param storeId
   * @param productSummaryRequest
   * @return
   */
  Page<ProductSolr> getL3ProductSummaryByProductSummaryRequest(String storeId,
      ProductSummaryRequestVo productSummaryRequest, PageRequest pageRequest);

  /**
   * Get L3 products for reels by ReelProductListingRequest
   *
   * @param storeId
   * @param reelProductListingRequestVo
   * @return
   */
  Page<ProductSolr> getL3ProductsForReelsByReelProductListingRequest(String storeId,
      ReelProductListingRequestVo reelProductListingRequestVo, PageRequest pageRequest);

  /**
   * Get product name suggestion
   *
   *
   * @param storeId
   * @param productSummaryRequest
   * @return
   */
  Page<ProductSolr> getProductNameByProductSummaryRequest(String storeId, ProductSummaryRequestVo productSummaryRequest,
      PageRequest pageRequest);

  /**
   * get count for active and oos products from l3 collection
   * @param merchantCode
   * @return
   */
  ProductCountResponseVo getActiveAndOosProductCount(String merchantCode);

  /**
   * get count for suspended and archived products from l3 collection
   *
   * @param merchantCode
   * @param productCountResponseVo
   * @return
   */
  ProductCountResponseVo getSuspendedAndArchivedProductCount(String merchantCode,
      ProductCountResponseVo productCountResponseVo);

  /**
   * fetch product summary response from l3 solr based on filters V2
   * @param storeId
   * @param page
   * @param size
   * @param productSummaryRequestV2Vo
   * @return
   */
  Page<ProductSummaryResponseV2Vo> getProductSummaryV2(String storeId, int page, int size, ProductSummaryRequestV2Vo productSummaryRequestV2Vo);

  /**
   * Execute atomic update
   *
   * @param solrInputDocuments
   * @throws Exception
   */
  void executeSolrDocumentsAtomicUpdate(List<SolrInputDocument> solrInputDocuments);

  /**
   * @param storeId        should not be null
   * @param merchantCode
   * @param productSkuList L3 list should not be empty
   */
  List<ProductSolr> findByProductSkuList(String storeId, String merchantCode, Set<String> productSkuList);

  /**
   * get products by store id and sizeChartCode
   *
   * @param sizeChartCode
   * @return
   */
  Page<ProductSkuSizeChartResponse> getActiveProductsByStoreIdAndSizeChartCode(String sizeChartCode, int page,
      int size);

  /**
   *get products details from solr by product sku list
   *
   * @param storeId
   * @param merchantCode
   * @param productSkus
   * @return
   */
  List<ProductSolr> findByProductSkuListForMFDTrueAndFalse(String storeId, String merchantCode,
      Set<String> productSkus);

}
