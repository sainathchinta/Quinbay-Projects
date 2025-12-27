package com.gdn.partners.pbp.service.productlevel1;

public interface ProductLevel1SolrService {

  void update(String productCode) throws Exception;

  /**
   * delete directly by productCollection Id
   * @param productCollectionId
   * @throws Exception
   */
  void deleteByProductCollectionId(String productCollectionId) throws Exception;

}
