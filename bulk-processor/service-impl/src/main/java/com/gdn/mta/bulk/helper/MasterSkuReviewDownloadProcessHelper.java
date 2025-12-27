package com.gdn.mta.bulk.helper;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterSkuInReviewDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.InReviewAnchorDownloadResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class MasterSkuReviewDownloadProcessHelper extends BulkProcessHelper {

  @Autowired
  private FileStorageOperationsService fileStorageOperationsService;

  private static final List<String> HEADER_LIST =
    ImmutableList.<String>builder().add(BulkParameters.FIRST_ANCHOR_SKU_CODE)
      .add(BulkParameters.FIRST_ANCHOR_SKU_NAME).add(BulkParameters.SECOND_ANCHOR_SKU_CODE)
      .add(BulkParameters.SECOND_ANCHOR_SKU_NAME).build();

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) throws Exception {
    return HEADER_LIST;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse bulkDataResponse) {
    BulkMasterSkuInReviewDownloadResponse masterSkuInReviewDownloadResponse =
      (BulkMasterSkuInReviewDownloadResponse) bulkDataResponse;
    List<List<String>> rowData = new ArrayList<>();
    for (InReviewAnchorDownloadResponse response : masterSkuInReviewDownloadResponse.getAnchorDownloadResponseList()) {
      List<String> row =
        Arrays.asList(response.getFirstAnchorSku(), response.getFirstAnchorSkuName(),
          response.getSecondAnchorSku(), response.getSecondAnchorSkuName());
      rowData.add(row);
    }
    return rowData;
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BulkMasterSkuInReviewDownloadResponse masterSkuInReviewDownloadResponse =
      (BulkMasterSkuInReviewDownloadResponse) response;
    return masterSkuInReviewDownloadResponse.getAnchorDownloadResponseList().size();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(request.getUsername()));
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM,
      EmailConstants.MASTER_SKU_IN_REVIEW_DOWNLOAD_TEMPLATE_ID);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM,
      EmailConstants.MASTER_SKU_IN_REVIEW_DOWNLOAD_TEMPLATE_SUBJECT);
    emailParameters.put(EmailConstants.FILE_PREFIX,
      fileStorageOperationsService.getFilePrefix(BulkProcessType.MASTER_SKU_IN_REVIEW_DOWNLOAD.getValue()));
    return emailParameters;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.MASTER_SKU_IN_REVIEW_DOWNLOAD)
      + request.getRequestId();
  }
}
