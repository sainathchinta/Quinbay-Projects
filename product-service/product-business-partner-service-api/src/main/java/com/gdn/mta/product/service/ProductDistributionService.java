package com.gdn.mta.product.service;

import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;

public interface ProductDistributionService {
  /**
   * remove product from PDT after product activation
   *
   * @param requestId
   * @param username
   * @param request
   * @throws Exception
   */
  void removeProductFromPDT(String requestId, String username, RemoveProductRequest request)
      throws Exception;
}
