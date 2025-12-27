package com.gdn.mta.bulk.helper;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkAutoApprovedProductsDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.AutoApprovedProductsResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
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
public class AutoApprovedProductsDownloadProcessHelper extends BulkProcessHelper{

  @Autowired
  private FileStorageOperationsService fileStorageOperationsService;

  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.PRODUCT_CODE)
          .add(BulkParameters.PRODUCT_NAME).add(BulkParameters.CATEGORY)
          .add(BulkParameters.STORE_NAME).add(BulkParameters.ASSIGNEE).build();

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) throws Exception {
    return HEADER_LIST;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse bulkDataResponse) {
    BulkAutoApprovedProductsDownloadResponse bulkAutoApprovedProductsDownloadResponse =
        (BulkAutoApprovedProductsDownloadResponse) bulkDataResponse;
    List<List<String>> rowData = new ArrayList<>();
    for (AutoApprovedProductsResponse response : bulkAutoApprovedProductsDownloadResponse.getAutoApprovedProductsResponses()) {
      List<String> row =
          Arrays.asList(response.getProductCode(), response.getProductName(),
              response.getCategory(), response.getStoreName(), response.getAssignedTo());
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
    BulkAutoApprovedProductsDownloadResponse bulkAutoApprovedProductsDownloadResponse =
        (BulkAutoApprovedProductsDownloadResponse) response;
    return bulkAutoApprovedProductsDownloadResponse.getAutoApprovedProductsResponses().size();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(request.getUsername()));
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM,
        EmailConstants.AUTO_APPROVED_PRODUCTS_DOWNLOAD_TEMPLATE_ID);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM,
        EmailConstants.AUTO_APPROVED_PRODUCTS_DOWNLOAD_TEMPLATE_SUBJECT);
    emailParameters.put(EmailConstants.FILE_PREFIX,
        fileStorageOperationsService.getFilePrefix(BulkProcessType.AUTO_APPROVED_PRODUCTS_DOWNLOAD.getValue()));
    return emailParameters;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(request.getBulkProcessEntity()) + request.getRequestId();
  }
}
