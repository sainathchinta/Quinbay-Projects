package com.gdn.mta.product.service.solr;

import java.io.IOException;
import java.util.Map;

import org.apache.solr.client.solrj.SolrServerException;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.DalamProductListRequest;
import com.gdn.mta.product.entity.ProductBusinessPartnerMapper;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountResponse;

public interface SolrReviewProductCollectionService {

  /**
   * Full reindex solr review product collection
   * @param storeId
   * @param isScreeningReindex
   * @throws Exception
   */
  void fullReindexCollection(String storeId, boolean isScreeningReindex) throws Exception;

  /**
   * Delta reindex solr review product collection
   * @param storeId
   * @throws Exception
   */
  void deltaReindexCollection(String storeId) throws Exception;
  /**
   * Delta reindex productCode in solr review product collection
   * @param storeId
   * @throws Exception
   */
  void deltaReindexProductCode(String storeId, String productCode) throws Exception;

  /**
   * Add product to review product collection on product creation, product resubmission
   *
   * @param productCollection
   * @throws Exception
   */
  void publishKafkaEventToAddProductToReviewProductCollection(ProductCollection productCollection) throws Exception;

  /**
   * Add product to review product collection while updating the product
   *
   * @param productCollection
   * @throws Exception
   */
  void addProductToReviewProductCollection(ProductCollection productCollection) throws Exception;

  /**
   * Delete product from review product collection on rejection, merge, need correction
   *
   * @param id
   */
  void deleteProductFromReviewProductCollection(String id) ;

  /**
   * Update assignedTo field in review product collection solr
   *
   * @param id
   * @param assignedTo
   */
  void updateAssignedToInReviewProductCollection(String id, String assignedTo);

  /**
   * Update brandApproved in solr on approval of brand
   * @param id
   * @param brandApproved
   * @param brandName
   */
  void updateBrandApprovedInReviewProductCollection(String id, boolean brandApproved, String brandName);

  /**
   * @param storeId
   * @param keyword
   * @param businessPartnerCode
   * @param categoryCode
   */
  ProductCollectionCountResponse getInReviewProducts(String storeId, String keyword, String businessPartnerCode,
      String categoryCode) throws IOException, SolrServerException;

  /**
   * filter Product tBusinessPartner Mapper By Activated And Viewable
   * @param storeId
   * @param activated
   * @param viewable
   * @param isSearch
   * @param searchCriteria
   * @param pageable
   * @return
   */
  Page<ProductBusinessPartnerMapper> findProductBusinessPartnerMapper(String storeId, boolean activated,
      boolean viewable, boolean isSearch, String searchCriteria, Pageable pageable) throws Exception;

  /**
   * find the dalam process products
   *
   * @param dalamProductListRequest
   * @return
   */
  Page<ProductCollection> findProductsForDalamProcess(DalamProductListRequest dalamProductListRequest,
      Pageable pageable) throws IOException, SolrServerException;
}
