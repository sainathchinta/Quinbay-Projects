package com.gdn.partners.pcu.internal.service;

import org.springframework.data.domain.Page;

import com.gdn.partners.pcu.internal.web.model.request.DistributionFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductVendorWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;


public interface DistributionListService {

  /**
   * API to fetch dangerous goods level data
   *
   * @param page
   * @param size
   * @param distributionFilterWebRequest
   */
  Page<DistributionProductWebResponse> getSummaryByMultipleFilter(int page, int size,
      DistributionFilterWebRequest distributionFilterWebRequest);

  /**
   * API to save product vendor mapping in PDT
   *
   * @Param productVendorWebRequest
   */
  void saveProductVendorMapping(ProductVendorWebRequest productVendorWebRequest);
}
