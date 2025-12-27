package com.gdn.x.product.service.api;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;

public interface CategoryService {


  /**
   * @param storeId
   * @param catalogCode
   * @param categoryCode
   * @param searchEmptySalesOnly
   * @throws
   * @return
   */
  Page<ProductAndItemSolr> getProductsByMasterCatalog(String storeId, String catalogCode,
      String categoryCode, boolean searchEmptySalesOnly, Pageable page) throws Exception;

  /**
   * @param storeId
   * @param catalogCode
   * @param categoryCode
   * @throws
   * @return
   */
  Page<ProductAndItemSolr> getProductsBySalesCatalog(String storeId, String catalogCode,
      String categoryCode, Pageable page) throws Exception;

  /**
   *
   * @param categoryCode
   * @return
   * @throws Exception
   */
  ProductScoreRuleResponse getProductScoreRuleForCategory(String categoryCode) throws Exception;

}