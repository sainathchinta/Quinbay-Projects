package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.feign.ProductAnalyticsFeign;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedWebRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedUserFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedListWebResponse;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedUserFeedbackResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.AutoApprovedService;
import com.gdn.partners.pcu.internal.service.VendorService;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;

import com.gdn.partners.pcu.internal.client.model.response.ProductAssigneeChangeResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
public class AutoApprovedServiceImpl implements AutoApprovedService {

  @Autowired
  private ProductAnalyticsFeign productAnalyticsFeign;

  @Autowired
  private VendorService vendorService;

  @Override
  public GdnRestListResponse<AutoApprovedListWebResponse> getAutoApprovedProductsList(int page, int size,
      AutoApprovedWebRequest request) {
    AutoApprovedRequest autoApprovedRequest = createAutoApprovedRequest(request);
    GdnRestListResponse<AutoApprovedListWebResponse> response;
    response = productAnalyticsFeign.fetchAutoApprovedProductsList(page, size, autoApprovedRequest);
    ResponseHelper.validateMasterSkuResponse(response);
    return response;
  }

  @Override
  public GdnRestListResponse<ProductAssigneeChangeResponse> updateAssignee(
      AutoApprovedAssigneeRequest assigneeRequest) throws Exception {
    if (StringUtils.isNotBlank(assigneeRequest.getAssigneeTo())) {
      validateUserNameAssigned(assigneeRequest.getAssigneeTo());
    }
    GdnRestListResponse<ProductAssigneeChangeResponse> response =
        productAnalyticsFeign.updateAssignedTo(assigneeRequest);
    ResponseHelper.validateMasterSkuResponse(response);
    return response;
  }

  @Override
  public GdnBaseRestResponse updateUserFeedback(String productCode, AutoApprovedUserFeedbackRequest autoApprovedUserFeedbackRequest) {
    GdnBaseRestResponse response = productAnalyticsFeign.updateUserFeedback(productCode,
        autoApprovedUserFeedbackRequest);
    ResponseHelper.validateMasterSkuResponse(response);
    return response;
  }

  @Override
  public GdnRestSingleResponse<AutoApprovedUserFeedbackResponse> fetchUserFeedback(String productCode) {
    GdnRestSingleResponse<AutoApprovedUserFeedbackResponse> response = productAnalyticsFeign.fetchUserFeedback(productCode);
    ResponseHelper.validateMasterSkuResponse(response);
    return response;
  }

  private AutoApprovedRequest createAutoApprovedRequest(AutoApprovedWebRequest request) {
    Boolean b2bActivated = null;
    if (request.isB2bActivated() && !request.isB2cActivated()) {
      b2bActivated = true;
    } else if (!request.isB2bActivated() && request.isB2cActivated()) {
      b2bActivated = false;
    }
    return AutoApprovedRequest.builder().keyword(request.getKeyword())
      .categoryCode(request.getCategoryCode()).sellerCode(request.getSellerCode())
      .sortOrder(request.getSortOrder()).assignedTo(request.getAssignedTo())
      .b2bActivated(b2bActivated).build();
  }

  private void validateUserNameAssigned(String userName) throws Exception {
    List<String> usernameList = vendorService.getProductReviewers();
    if (!usernameList.contains(userName)) {
      throw new InvalidStateException(ErrorMessages.INVALID_USER_NAME);
    }
  }
}
