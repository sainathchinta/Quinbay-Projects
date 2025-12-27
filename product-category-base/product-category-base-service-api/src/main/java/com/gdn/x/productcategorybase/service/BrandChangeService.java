package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.entity.brand.Brand;

public interface BrandChangeService {

  /**
   * Create solr document for new brand and add it to solr
   *
   * @param brand
   * @throws Exception
   */
  void createSolrDocumentForBrandCollection(Brand brand) throws Exception;

  /**
   * Delete the brand entry from Solr when brand deletion operation is performed
   *
   * @param brandCode
   * @param storeId
   * @throws Exception
   */
  void deleteSolrDocumentFromBrandCollection(String brandCode, String storeId) throws Exception;

  /**
   * Update solr on brand update
   *
   * @param id
   * @param protectedBrand
   */
  void updateProtectedBrand(String id , boolean protectedBrand);
}
