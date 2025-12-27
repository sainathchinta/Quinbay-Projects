package com.gdn.partners.pcu.internal.service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedWebRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedUserFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedListWebResponse;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedUserFeedbackResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductAssigneeChangeResponse;

public interface AutoApprovedService {

  /**
   * to fetch auto approved products from product analytics
   *
   * @param page
   * @param size
   * @param request
   * @return list of auto approved products
   */
  GdnRestListResponse<AutoApprovedListWebResponse> getAutoApprovedProductsList(int page, int size,
      AutoApprovedWebRequest request);

  /**
   * Update assignee
   * @param assigneeRequest Web request
   * @return ProductAssigneeChangeResponse list
   */
  GdnRestListResponse<ProductAssigneeChangeResponse> updateAssignee(
    AutoApprovedAssigneeRequest assigneeRequest) throws Exception;

  /**
   * update user feedback for auto approved product
   *
   * @param productCode         productCode
   * @param autoApprovedUserFeedbackRequest userFeedbackRequest
   * @return GdnBaseRestResponse
   */
  GdnBaseRestResponse updateUserFeedback(String productCode, AutoApprovedUserFeedbackRequest autoApprovedUserFeedbackRequest);

  /**
   * fetch user feedback of auto approved product
   *
   * @param productCode productCode
   * @return GdnRestSingleResponse<UserFeedbackResponse>
   */
  GdnRestSingleResponse<AutoApprovedUserFeedbackResponse> fetchUserFeedback(String productCode);
}
