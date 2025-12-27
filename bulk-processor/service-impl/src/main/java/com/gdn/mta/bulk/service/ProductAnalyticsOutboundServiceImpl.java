package com.gdn.mta.bulk.service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.feignConfig.ProductAnalyticsFeign;
import com.gdn.mta.bulk.models.AutoApprovedAssigneeRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.AutoApprovedSelectedDownloadRequest;
import com.gdn.mta.bulk.models.download.AutoApprovedWebRequest;
import com.gdn.mta.bulk.models.download.responsedata.AutoApprovedListWebResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductAssigneeChangeResponse;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

@Slf4j
@Service
public class ProductAnalyticsOutboundServiceImpl implements ProductAnalyticsOutboundService {

  @Autowired
  private ProductAnalyticsFeign productAnalyticsFeign;

  @Override
  public String processUpdateAssigneeForAutoApprovedProducts(String requestId,
      AutoApprovedAssigneeRequest autoApprovedAssigneeRequest) {
    GdnRestListResponse<ProductAssigneeChangeResponse>
        response = productAnalyticsFeign.updateAssignedTo(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, requestId, Constant.USER_NAME, autoApprovedAssigneeRequest);
    return validateUpdateAssigneeResponse(response, autoApprovedAssigneeRequest);
  }

  private String validateUpdateAssigneeResponse(
    GdnRestListResponse<ProductAssigneeChangeResponse> response,
    AutoApprovedAssigneeRequest autoApprovedAssigneeRequest) {
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    if (CollectionUtils.isNotEmpty(response.getContent())) {
      log.error(
        "Error on updating assignee of auto approved products for request {}, " + "error : {}",
        autoApprovedAssigneeRequest, response.getContent().get(0).getErrorMessage());
      return response.getContent().get(0).getErrorMessage();
    }
    return null;
  }

  @Override
  public List<AutoApprovedListWebResponse> fetchAutoApprovedProductsDownloadList(String requestId,
      int page, int size, AutoApprovedWebRequest request) {
    GdnRestListResponse<AutoApprovedListWebResponse>
        autoApprovedListWebResponseGdnRestListResponse =
        productAnalyticsFeign.fetchAutoApprovedProductsList(Constant.STORE_ID, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, requestId, Constant.USER_NAME, page, size, request);
    validateResponse(autoApprovedListWebResponseGdnRestListResponse, request);
    return autoApprovedListWebResponseGdnRestListResponse.getContent();
  }

  @Override
  public List<AutoApprovedListWebResponse> fetchAutoApprovedProductsSelectedDownloadList(
      String requestId, AutoApprovedSelectedDownloadRequest request) {
    GdnRestListResponse<AutoApprovedListWebResponse>
        autoApprovedListWebResponseGdnRestListResponse =
        productAnalyticsFeign.fetchAutoApprovedProductsSelectedDownload(Constant.STORE_ID,
            Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId, Constant.USER_NAME, request);
    validateResponse(autoApprovedListWebResponseGdnRestListResponse, request);
    return autoApprovedListWebResponseGdnRestListResponse.getContent();
  }

  void validateResponse(GdnRestListResponse<AutoApprovedListWebResponse> response, Object request) {
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    if (!response.isSuccess()) {
      log.error("Error on fetching auto approved products download for request {}, " + "error : {}",
          request, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }
}
