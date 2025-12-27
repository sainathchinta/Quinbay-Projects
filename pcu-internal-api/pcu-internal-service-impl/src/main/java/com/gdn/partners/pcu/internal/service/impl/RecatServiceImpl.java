package com.gdn.partners.pcu.internal.service.impl;

import com.gda.mta.product.dto.response.SequenceResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.RecatService;
import com.gdn.partners.pcu.internal.service.XBulkOutboundService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.internal.web.model.request.RecatProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RecatProductSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.RecatProcessSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductSummaryWebResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

@Slf4j
@Service
public class RecatServiceImpl implements RecatService {

  private static final String DOT = ".";
  private static final DateFormat format = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
  private static final int PADDING_COUNT = 7;
  private static final char PADDING_CONSTANT = '0';

  @Autowired
  private XBulkOutboundService xBulkOutboundService;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private SystemParameterProperties systemParameterProperties;

  @Override
  public void uploadRecatRequest(MultipartFile recatExcel, String scheduledTime) throws Exception {
    RequestHelper.checkRecatAccessibility(Credential.getAccessibilities());
    Date recatScheduleTime = getScheduledDate(scheduledTime);
    validateUploadExcelType(recatExcel);
    GdnRestSingleResponse<SequenceResponse> recatRequestResponse =
        this.pbpFeign.findCounterByKey(Constants.RECAT_REQUEST_CODE_KEY);
    ResponseHelper.validateResponse(recatRequestResponse);
    String recatRequestCode =
        new StringBuilder().append(Constants.RECAT_REQUEST_CODE_KEY).append(Constants.HYPHEN)
            .append(StringUtils
                .leftPad(String.valueOf(recatRequestResponse.getValue().getCounter()), PADDING_COUNT, PADDING_CONSTANT))
            .toString();
    generatePathAndUploadFile(recatExcel, recatRequestCode);
    xBulkOutboundService
        .uploadNewRecatRequest(recatRequestCode, recatExcel.getOriginalFilename(), format.format(recatScheduleTime));
  }


  @Override
  public void getFailedProductsMail(String recatRequestCode) {
    RequestHelper.checkRecatAccessibility(Credential.getAccessibilities());
    xBulkOutboundService.getFailedProductsMail(recatRequestCode);
  }

  private void generatePathAndUploadFile(MultipartFile recatExcel, String recatRequestCode) throws Exception {
    fileStorageService.uploadFilePath(recatExcel, recatRequestCode, BulkInternalProcessType.INTERNAL_RECAT.getValue());
  }

  private void validateUploadExcelType(MultipartFile recatExcel) {
    if (!FileType.XLSX.name().equalsIgnoreCase(recatExcel.getOriginalFilename()
        .substring(recatExcel.getOriginalFilename().lastIndexOf(DOT) + 1))) {
      log.error("Input file is of invalid type, should be XLSX. File uploaded  - {}",
          recatExcel.getOriginalFilename());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
          ErrorMessages.XLSX_TYPE_ONLY);
    }
  }

  private Date getScheduledDate(String scheduledTime) {
    Date recatScheduledTime = new Date();
    if (StringUtils.isNotEmpty(scheduledTime)) {
      try {
        recatScheduledTime = format.parse(scheduledTime);
      } catch (ParseException e) {
        log.error("Error parsing input schedule time string : {}, error - ", scheduledTime, e);
        return recatScheduledTime;
      }
    }
    return recatScheduledTime;
  }

  @Override
  public Page<RecatProcessSummaryWebResponse> getRecatSummaryByFilter(
      RecatProcessSummaryWebRequest recatProcessSummaryWebRequest, int page, int size)
      throws Exception {
    RequestHelper.checkRecatAccessibility(Credential.getAccessibilities());
    Page<RecatProcessSummaryResponse> recatProcessSummaryResponses = xBulkOutboundService
        .recatProcessFilterSummary(
            RequestHelper.toRecatProcessSummaryRequest(recatProcessSummaryWebRequest), page, size);
    return ResponseHelper.toRecatProcessSummaryWebResponsePage(recatProcessSummaryResponses);
  }

  @Override
  public void cancelRecatRequest(String recatRequestCode) {
    RequestHelper.checkRecatAccessibility(Credential.getAccessibilities());
    xBulkOutboundService.cancelRecatRequest(recatRequestCode);
  }


  public Page<RecatProductSummaryWebResponse> getRecatProductSummary(String recatRequestCode, int page, int size,
      RecatProductSummaryWebRequest recatProductSummaryWebRequest) {
    RequestHelper.checkRecatAccessibility(Credential.getAccessibilities());
      Page<RecatProductSummaryResponse> recatProductSummaryResponsePage = xBulkOutboundService
          .getRecatProductSummaryByRecatRequestCode(recatRequestCode, page, size,
              RequestHelper.toRecatProductSummaryRequest(recatProductSummaryWebRequest));
      List<RecatProductSummaryWebResponse> recatProductSummaryWebResponseList =
          ResponseHelper.toRecatProductSummaryWebResponse(recatProductSummaryResponsePage.getContent());
      return new PageImpl<>(recatProductSummaryWebResponseList, PageRequest.of(page, size),
          recatProductSummaryResponsePage.getTotalElements());
  }

  @Override
  public RecatProductCountWebResponse getRecatProductStatusCounts(String recatRequestCode) {
    RequestHelper.checkRecatAccessibility(Credential.getAccessibilities());
    RecatProductCountResponse recatProductCountResponse =
        xBulkOutboundService.getRecatProductStatusCount(recatRequestCode);
    return ResponseHelper.toRecatProductCountWebResponse(recatProductCountResponse);
  }
}
