package com.gdn.partners.product.analytics.service;

import com.gdn.partners.product.analytics.web.model.UserFeedbackResponse;
import com.gdn.partners.product.analytics.web.model.request.UserFeedbackRequest;

public interface UserFeedbackService {

  /**
   * to update user feedback for auto approved product
   *
   * @param productCode         productCode
   * @param userFeedbackRequest userFeedbackRequest
   */
  void updateUserFeedbackForAutoApprovedProduct(String productCode, UserFeedbackRequest userFeedbackRequest)
      throws Exception;

  /**
   * to fetch user feedback response
   *
   * @param productCode productCode
   * @return UserFeedbackResponse
   * @throws Exception Exception
   */
  UserFeedbackResponse fetchUserFeedbackResponse(String productCode) throws Exception;
}
