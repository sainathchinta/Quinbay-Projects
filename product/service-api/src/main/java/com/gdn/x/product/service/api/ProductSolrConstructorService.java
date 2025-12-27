package com.gdn.x.product.service.api;

import java.util.Map;

import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.rest.web.model.FieldValueObject;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;

public interface ProductSolrConstructorService {

  /**
   *
   * @param productSolr
   * @param product
   * @param getMasterData
   */
  void constructProduct(ProductSolr productSolr, Product product, boolean getMasterData);

  /**
   * @param productSolr
   * @param productDomainEventModel
   * @param productAndTotalScoreMap
   */
  Map<String, FieldValueObject> constructProductFromMasterDataChanges(ProductSolr productSolr,
      ProductDomainEventModel productDomainEventModel, Map<String, Double> productAndTotalScoreMap);

  /**
   *
   * @param catalogString
   * @return
   */
  MasterCatalog constructMasterCatalog(String catalogString);

}
