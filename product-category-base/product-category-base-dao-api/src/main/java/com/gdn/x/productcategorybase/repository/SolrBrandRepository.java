package com.gdn.x.productcategorybase.repository;

import java.io.IOException;
import java.util.List;

import org.apache.solr.client.solrj.SolrServerException;

import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;

public interface SolrBrandRepository {

  /**
   * Add brands to brand collection
   *
   * @param solrBrandModels
   */
  void addBrandsToBrandCollectionSolr(List<SolrBrandModel> solrBrandModels);

  /**
   * Delete brand from brand collection solr
   *
   * @param ids
   */
  void deleteBrandsFromBrandCollectionSolr(List<String> ids);

  /**
   * Update brand on approval/rejection
   *
   * @param solrBrandModels
   */
  void updateBrandApprovalStatusListInSolr(List<SolrBrandModel> solrBrandModels);

  /**
   * Update brand name in solr on updating brand
   *  @param id
   * @param brandName
   * @param protectedBrand
   */
  void updateBrandNameInSolr(String id, String brandName, boolean protectedBrand) throws IOException, SolrServerException;

  /**
   * Delete all documents from solr
   *
   */
  void deleteAllDocumentsFromSolr() throws IOException, SolrServerException;

}
