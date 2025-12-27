package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.product.UploadProcessCount;
import com.gdn.partners.pcu.internal.client.feign.XBulkFeign;
import com.gdn.partners.pcu.internal.client.model.response.StringResponse;
import com.gdn.partners.pcu.internal.service.XBulkOutboundService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.mta.bulk.dto.SimpleStringResponse;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class XBulkOutboundServiceImpl implements XBulkOutboundService {

  @Autowired
  private XBulkFeign xBulkFeign;

  @Override
  public UploadProcessCount getPendingProcessCount(String bulkProcessType, String status) {
    GdnRestSingleResponse<UploadProcessCount> response = xBulkFeign.countNumberOfUploads(bulkProcessType, status);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public void getFailedProductsMail(String recatRequestCode){
    GdnBaseRestResponse response = xBulkFeign.getFailedProductsMail(recatRequestCode);
  }

  @Override
  public Page<RecatProcessSummaryResponse> recatProcessFilterSummary(
      RecatProcessSummaryRequest recatProcessSummaryRequest, int page, int size) {
    GdnRestListResponse<RecatProcessSummaryResponse> response =
        xBulkFeign.getRecatProcessSummary(recatProcessSummaryRequest, page, size);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void uploadNewRecatRequest(String recatRequestCode, String fileName,
      String scheduledTime) {
    GdnBaseRestResponse response =
        xBulkFeign.uploadNewRecatRequest(recatRequestCode, fileName, scheduledTime);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void cancelRecatRequest(String recatRequestCode) {
    GdnBaseRestResponse response = xBulkFeign.cancelRecatRequest(recatRequestCode, false);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public Page<RecatProductSummaryResponse> getRecatProductSummaryByRecatRequestCode(String recatRequestCode, int page, int size,
      RecatProductSummaryRequest recatProductSummaryRequest) {
    GdnRestListResponse<RecatProductSummaryResponse> response =
        xBulkFeign.getProductSummary(recatRequestCode, page, size, recatProductSummaryRequest);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public RecatProductCountResponse getRecatProductStatusCount(String recatRequestCode) {
    GdnRestSingleResponse<RecatProductCountResponse> response = xBulkFeign.getProductStatusCounts(recatRequestCode);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public String downloadStoreCopyUploadTemplate(String sellerCode) {
    GdnRestSingleResponse<StringResponse> singleResponse =
        xBulkFeign.downloadUploadTemplateForStoreCopy(sellerCode);
    ResponseHelper.validateResponse(singleResponse);
    return singleResponse.getValue().getValue();
  }

  @Override
  public BulkInternalPendingRequestResponse getPendingProcesses(String sellerCode, String userName,
      String processType) {
    GdnRestSingleResponse<BulkInternalPendingRequestResponse> singleResponse =
        xBulkFeign.getPendingBulkRequests(sellerCode, userName, processType);
    ResponseHelper.validateResponse(singleResponse);
    return singleResponse.getValue();
  }

  @Override
  public InternalProcessPendingFilesResponse checkPendingFilesForAutoAssignment(String storeId, String userName, String processType) {
    GdnRestSimpleResponse<InternalProcessPendingFilesResponse> response =
            xBulkFeign.checkPendingFiles(storeId, userName, processType);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }
}
