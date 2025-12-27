package com.gdn.x.mta.distributiontask.service.api;

import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;

public interface ProductImageQcFeedbackService {

  /**
   * Save image qc feedback
   * @param productImageQcFeedbackRequest
   * @param updateUserFeedback
   * @param updateSystemFeedback
   *
   */
  void upsertImageQcFeedback(ProductImageQcFeedbackRequest productImageQcFeedbackRequest, boolean updateUserFeedback,
      boolean updateSystemFeedback) throws Exception;

  /**
   * Get product image feedback
   *
   * @param storeId
   * @param productCode
   * @return
   */
  ProductImageQcFeedbackResponse findProductQcFeedbackResponseByProductCode(String storeId, String productCode);
}