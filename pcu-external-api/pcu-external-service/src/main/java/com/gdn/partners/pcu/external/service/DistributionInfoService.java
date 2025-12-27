package com.gdn.partners.pcu.external.service;

import com.gdn.partners.pcu.external.web.model.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import org.springframework.data.domain.Page;

public interface DistributionInfoService {

  /**
   * Fetches paged distribution info for SKUs of a product.
   *
   * @param productCode product identifier
   * @param needDistributionInfoResponse identifier to fetch product
   * @param page page number for pagination
   * @param size size of each page
   * @return paged distribution info responses
   */
  Page<DistributionInfoPerSkuResponse> getDistributionInfo(String productCode,
      boolean needDistributionInfoResponse, int page, int size);

  /**
   * update distribution info
   *
   * @param productCode
   * @param distributionInfoUpdateRequest
   */
  void updateDistributionInfo(String productCode,
      DistributionInfoUpdateRequest distributionInfoUpdateRequest);
}
