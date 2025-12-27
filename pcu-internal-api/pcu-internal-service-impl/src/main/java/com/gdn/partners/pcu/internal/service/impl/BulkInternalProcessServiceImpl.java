package com.gdn.partners.pcu.internal.service.impl;

import static com.gdn.partners.pcu.internal.model.Constants.DOT;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.gda.mta.product.dto.response.SequenceResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.XBulkFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.BulkInternalProcessService;
import com.gdn.partners.pcu.internal.service.FileHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.internal.web.model.request.BulkInternalProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BulkInternalProcessSummaryWebResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BulkInternalProcessServiceImpl implements BulkInternalProcessService {

  @Autowired
  private XBulkFeign xBulkFeign;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private FileHelper fileHelper;

  @Override
  public Page<BulkInternalProcessSummaryWebResponse> bulkInternalProcessSummary(BulkInternalProcessSummaryWebRequest bulkInternalProcessSummaryWebRequest,
      int page, int size) throws Exception {
    GdnRestListResponse<BulkInternalProcessSummaryResponse> bulkInternalProcessSummary =
        xBulkFeign.bulkInternalProcessSummary(RequestHelper.toBulkInternalProcessSummaryRequest(bulkInternalProcessSummaryWebRequest), page, size);
    ResponseHelper.validateResponse(bulkInternalProcessSummary);
    List<BulkInternalProcessSummaryWebResponse> bulkInternalProcessSummaryWebResponseList =
        ResponseHelper.toBulkInternalProcessSummaryWebResponse(bulkInternalProcessSummary.getContent());
    return new PageImpl<>(bulkInternalProcessSummaryWebResponseList, PageRequest.of(page, size),
        bulkInternalProcessSummary.getPageMetaData().getTotalRecords());
  }

  @Override
  public void uploadInternalProcess(MultipartFile internalProcessExcel, String sellerCode, String sellerName,
      String processType) throws Exception {
    RequestHelper.checkAccessibilityByProcessType(processType);
    RequestHelper.validateInternalUploadExcelType(processType, internalProcessExcel.getOriginalFilename());
    GdnRestSingleResponse<SequenceResponse> sequenceResponse =
        this.pbpFeign.findCounterByKey(Constants.BULK_INTERNAL_PROCESS_CODE_KEY);
    ResponseHelper.validateResponse(sequenceResponse);
    String bipRequestCode =
        new StringBuilder().append(Constants.BULK_INTERNAL_PROCESS_CODE_KEY).append(Constants.HYPHEN)
            .append(StringUtils
                .leftPad(String.valueOf(sequenceResponse.getValue().getCounter()), Constants.PADDING_COUNT, Constants.PADDING_CONSTANT))
            .toString();
    fileHelper.uploadFileBasedOnProcessType(internalProcessExcel, processType, bipRequestCode);
    xBulkFeign.uploadNewBulkInternalProcessRequest(
        RequestHelper.getUploadRequestByProcessType(processType, internalProcessExcel.getOriginalFilename(), sellerCode,
            sellerName, bipRequestCode));
  }

  @Override
  public void cancelInternalBulkProcessRequest(String internalProcessRequestCode) {
    GdnBaseRestResponse response =  xBulkFeign.bulkInternalProcessCancelRequest(internalProcessRequestCode);
    ResponseHelper.validateResponse(response);
  }


}
