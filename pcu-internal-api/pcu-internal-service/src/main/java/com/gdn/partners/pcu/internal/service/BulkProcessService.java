package com.gdn.partners.pcu.internal.service;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;

/**
 * Created by govind on 30/01/2019 AD.
 */
public interface BulkProcessService {

  /**
   * Bulk Download screening products
   * @param username
   * @param reviewProductsFilterRequest
   * @param lang
   */
  void bulkDownloadScreeningProducts(String username,
      ReviewProductsFilterRequest reviewProductsFilterRequest, String lang);
}
