package com.gdn.mta.product.service;

import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pbp.entity.productlevel3.CountProductLevel3Wip;

public interface ProductLevel3WipWrapper {

  /**
   * Find detail by product sku
   *
   * @param storeId
   * @param productSku
   * @param isActive
   * @return
   * @throws Exception
   */
  ProductLevel3WipDetailResponse findByProductSku(String storeId, String productSku, boolean isActive)
      throws Exception;

  /**
   * Get product counts by filter type
   *
   * @param businessPartnerCode String
   * @param storeId string
   * @param type primary secondary
   * @return totalItemsByCriteria , totalItems
   * @throws Exception
   */
  ProductLevel3CountResponse countSummaryByFilterType(String businessPartnerCode, String storeId,
    String type) throws Exception;

  /**
   * Get product counts by State
   *
   * @param businessPartnerCode String
   * @return totalItemsByCriteria , totalItems
   * @throws Exception
   */
  CountProductLevel3Wip countSummaryWithState(String businessPartnerCode) throws Exception;
}
