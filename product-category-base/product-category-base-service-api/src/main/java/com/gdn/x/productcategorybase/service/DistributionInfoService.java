package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Map;

import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface DistributionInfoService {

  /**
   * Updates the distribution information for a product identified by its code.
   *
   * @param storeId
   * @param productCode                   product code
   * @param distributionInfoUpdateRequest the request object containing distribution information
   * @param product                       complete product details
   *                                      to be updated
   * @return
   */
  List<ProductItem> updateDistributionInfo(String storeId, String productCode,
      DistributionInfoUpdateRequest distributionInfoUpdateRequest, Product product) throws Exception;


  /**
   * Get distribution information for a product identified by its code.
   *
   * @param storeId           String
   * @param productCode       String
   * @param pageable          pageable
   * @param skuCodeAndNameMap
   */
  Page<DistributionInfoPerSkuResponse> getDistributionInfo(String storeId, String productCode,
      Pageable pageable, Product product, Map<String, ProductItem> skuCodeAndNameMap);
}