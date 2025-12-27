package com.gdn.mta.bulk.helper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.service.FileStorageOperationsService;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkConfigSummaryResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.google.common.collect.ImmutableList;

@Component(value = "bulkConfigurationMerchantSummaryProcessHelper")
public class BulkConfigurationMerchantSummaryProcessHelper extends BulkProcessHelper {

  private static final List<String> MERCHANT_HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.CONFIG_SELLER_CODE).add(BulkParameters.CONFIG_SELLER_NAME)
          .add(BulkParameters.REVIEW_CONFIG).build();

  @Autowired
  private FileStorageOperationsService fileStorageOperationsService;

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) {
    return MERCHANT_HEADER_LIST;
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkConfigSummaryResponse bulkConfigSummaryResponse = (BulkConfigSummaryResponse) response;
    return getContents(bulkConfigSummaryResponse.getBulkConfigDataResponseList());
  }

  private List<List<String>> getContents(List<BulkConfigDataResponse> bulkConfigDataResponseList) {
    return bulkConfigDataResponseList.stream().map(this::getProductContents).collect(Collectors.toList());
  }

  private List<String> getProductContents(BulkConfigDataResponse bulkConfigDataResponse) {
    List<String> response = new ArrayList<>();
    response.add(bulkConfigDataResponse.getCode());
    response.add(bulkConfigDataResponse.getName());
    response.add(bulkConfigDataResponse.getReviewConfig());
    return response;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.CONFIGURATION_MERCHANT_SUMMARY) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(request.getUsername()));
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM,
        MessageUtil.getMessage(EmailConstants.BULK_MERCHANT_CONFIGURATION_DOWNLOAD_TEMPLATE_ID, lang));
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, EmailConstants.CONFIGURATION_SUMMARY_SELLER_SUBJECT);
    emailParameters.put(EmailConstants.FILE_PREFIX, fileStorageOperationsService.getEmailPrefix());
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BulkConfigSummaryResponse bulkConfigSummaryResponse = (BulkConfigSummaryResponse) response;
    return bulkConfigSummaryResponse.getBulkConfigDataResponseList().size();
  }
}
