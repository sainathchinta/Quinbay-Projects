package com.gdn.x.productcategorybase.solr.service;

import com.gdn.x.productcategorybase.solr.model.DeltaProduct;
import com.gdn.x.productcategorybase.solr.model.ProductModel;
import com.gdn.x.productcategorybase.solr.model.ReIndexType;
import org.apache.solr.client.solrj.SolrServerException;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by Kesha on 24/04/16.
 */
public interface ProductService {

  /**
   * Get All active Product Models
   *
   * @return
   */
  List<ProductModel> getAllProducts();

  /**
   * API to get data from DB and add in solr
   *
   * @param productModelList
   * @param categoryToParentMap
   * @param indexType
   * @throws IOException
   * @throws SolrServerException
   */
  void getDataAndPostDocumentsToSolr(List<ProductModel> productModelList,
      Map<String, String> categoryToParentMap, ReIndexType indexType) throws Exception;


  /**
   * Get Category to parent category mapping
   *
   * @return
   */
  Map<String, String> getCategoryToParentMap();

  /**
   * Update products in solr
   *  @param productChunk
   * @param productCategoriesMapping
   */
  void deltaUpdate(List<DeltaProduct> productChunk, Map<String, String> productCategoriesMapping) throws Exception;

  /**
   * Fetch Updated products after last reindex
   *
   * @return
   */
  Set<DeltaProduct> fetchUpdatedProducts();

  /**
   * Delete products which processed in reindexing job
   *
   */
  void deleteProcessedProducts();

  /**
   * Update state of Products to processing when solr reindex job is running
   *
   */
  void updateAllDetlaProductsToProcess();

  /**
   * updates all the products into Solr database
   *
   */
  boolean fullIndex();

  /**
   * Updates products which are modified after last reindex
   *
   */
  boolean deltaIndex();
}
