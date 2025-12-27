package com.gdn.mta.bulk.service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.feignConfig.MasterSkuReviewFeign;
import com.gdn.mta.bulk.models.download.ChangeAssigneeRequest;
import com.gdn.mta.bulk.models.download.responsedata.InReviewAnchorDownloadResponse;
import com.gdn.mta.bulk.models.download.DownloadInReviewAnchorsWebRequest;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

@Slf4j
@Service
public class MasterSkuReviewOutboundServiceImpl implements MasterSkuReviewOutboundService {

  @Autowired
  private MasterSkuReviewFeign masterSkuReviewFeign;

  public List<InReviewAnchorDownloadResponse> fetchAnchorMappingDownloadList(String requestId,
    int page, int size, DownloadInReviewAnchorsWebRequest request) {
    GdnRestListResponse<InReviewAnchorDownloadResponse> response =
      masterSkuReviewFeign.getInReviewAnchorsDownload(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, requestId, Constant.USER_NAME, page, size, request);
    validateAnchorMappingDownloadResponse(response, request);
    return response.getContent();
  }

  private void validateAnchorMappingDownloadResponse(GdnRestListResponse response,
    DownloadInReviewAnchorsWebRequest request) {
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    if (!response.isSuccess()) {
      log.error("Error on fetching anchor mapping details for in-review download for request {}, "
        + "error : {}", request, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  public String processBulkUploadAssigneeAction(String requestId, ChangeAssigneeRequest request) {
    GdnBaseRestResponse response =
      masterSkuReviewFeign.updateAssignedTo(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, requestId, request.getAssignedBy(), request);
    return validateUploadAssigneeResponse(response, request);
  }

  private String validateUploadAssigneeResponse(GdnBaseRestResponse response,
    ChangeAssigneeRequest request) {
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    if (!response.isSuccess()) {
      log.error("Error on fetching anchor mapping details for in-review download for request {}, "
        + "error : {}", request, response.getErrorMessage());
      return response.getErrorMessage();
    }
    return null;
  }
}
