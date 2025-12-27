package com.gdn.mta.bulk.helper;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkIprProductDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.IprProductsResponse;
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
public class IprProductsDownloadProcessHelper extends BulkProcessHelper {

  @Autowired
  private FileStorageOperationsService fileStorageOperationsService;

  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.IPR_PRODUCT_SKU)
          .add(BulkParameters.PRODUCT_NAME).add(BulkParameters.CATEGORY).add(BulkParameters.BRAND)
          .add(BulkParameters.IPR_SOURCE).add().build();

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) throws Exception {
    return HEADER_LIST;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse bulkDataResponse) {
    BulkIprProductDownloadResponse iprProductDownloadResponse =
        (BulkIprProductDownloadResponse) bulkDataResponse;
    List<List<String>> rowData = new ArrayList<>();
    for (IprProductsResponse response : iprProductDownloadResponse.getIprProductsResponses()) {
      List<String> row = Arrays.asList(response.getProductSku(), response.getProductName(),
          response.getCategoryName(), response.getBrandName(), response.getSource());
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
    BulkIprProductDownloadResponse bulkIprProductDownloadResponse =
        (BulkIprProductDownloadResponse) response;
    return bulkIprProductDownloadResponse.getIprProductsResponses().size();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(request.getUsername()));
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM,
        EmailConstants.BULK_IPR_PRODUCT_DOWNLOAD_TEMPLATE_ID);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM,
        EmailConstants.BULK_IPR_PRODUCT_DOWNLOAD_TEMPLATE_SUBJECT);
    emailParameters.put(EmailConstants.FILE_PREFIX,
        fileStorageOperationsService.getFilePrefix(BulkProcessType.IPR_PRODUCTS_DOWNLOAD_ALL.getValue()));
    return emailParameters;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.IPR_PRODUCTS_DOWNLOAD_ALL)
        + request.getRequestId();
  }
}
