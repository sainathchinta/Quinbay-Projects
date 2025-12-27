package com.gdn.mta.bulk.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.ItemSkuAndMasterSkuResponse;
import com.gdn.mta.bulk.models.download.responsedata.MasterSkuItemsDownloadResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class BulkMasterSkuReviewItemsDownloadProcessHelper extends BulkProcessHelper {
  private static final String ITEM_SKU = "SKU item";
  private static final String SKU_NAME = "Nama SKU item";
  private static final String MASTER_SKU = "Kode master SKU";
  private static final String MASTER_SKU_NAME = "Nama master SKU";

  @Autowired
  private FileStorageOperationsService fileStorageOperationsService;

  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(ITEM_SKU).add(SKU_NAME).add(MASTER_SKU).add(MASTER_SKU_NAME).build();

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) throws Exception {
    return HEADER_LIST;
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    MasterSkuItemsDownloadResponse masterSkuItemsDownloadResponse = (MasterSkuItemsDownloadResponse) response;
    List<List<String>> rowData = new ArrayList<>();
    for (ItemSkuAndMasterSkuResponse itemSkuAndMasterSkuResponse : masterSkuItemsDownloadResponse.getItemSkuAndMasterSkuResponseList()) {
      List<String> row =
          Arrays.asList(itemSkuAndMasterSkuResponse.getItemSku(), itemSkuAndMasterSkuResponse.getSkuName(),
              itemSkuAndMasterSkuResponse.getMasterItemSku(), itemSkuAndMasterSkuResponse.getMasterSkuName());
      rowData.add(row);
    }
    return rowData;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(request.getBulkProcessEntity()) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(request.getUsername()));
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM,
      EmailConstants.MASTER_SKU_REVIEW_ITEMS_DOWNLOAD_TEMPLATE_ID);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM,
      EmailConstants.MASTER_SKU_REVIEW_ITEMS_DOWNLOAD_SUBJECT);
    emailParameters.put(EmailConstants.FILE_PREFIX,
      fileStorageOperationsService.getFilePrefix(BulkProcessType.MASTER_SKU_REVIEW_ITEMS_DOWNLOAD.getValue()));
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    MasterSkuItemsDownloadResponse masterSkuItemsDownloadResponse = (MasterSkuItemsDownloadResponse) response;
    return masterSkuItemsDownloadResponse.getItemSkuAndMasterSkuResponseList().size();
  }
}
