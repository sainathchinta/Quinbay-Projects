package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface DistributionInfoWrapperService {

  /**
   * Updates the distribution information for a product publish
   *
   * @param storeId                       storeId
   * @param productCode                   product code
   * @param distributionInfoUpdateRequest the request object containing distribution information
   *                                      to be updated
   */
  void updateDistributionInfoAndPublishProduct(String storeId, String productCode,
      DistributionInfoUpdateRequest distributionInfoUpdateRequest) throws Exception;

  /**
   * Fetches paged distribution info for SKUs of a product.
   *
   * @param storeId String
   * @param productCode product identifier
   * @param pageable pagination and sorting info
   * @return paged distribution info responses
   */
  Page<DistributionInfoPerSkuResponse> getDistributionInfo(String storeId, String productCode, Pageable pageable);
}