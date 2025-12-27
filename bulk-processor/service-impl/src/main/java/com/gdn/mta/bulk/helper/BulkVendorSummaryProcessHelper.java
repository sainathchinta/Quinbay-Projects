package com.gdn.mta.bulk.helper;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import com.gdn.partners.bulk.util.Constant;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkVendorSummaryResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;

/**
 * Created by shivam on 09/07/2019
 */
@Component(value = "bulkVendorSummaryProcessHelper")
public class BulkVendorSummaryProcessHelper extends BulkProcessHelper {

  private static final String FINISHED = "Selesai";
  private static final String NOT_FINISIHED = "Belum";

  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.PRODUCT_CODE).add(BulkParameters.PRODUCT_NAME)
          .add(BulkParameters.CATEGORY).add(BulkParameters.STORE_NAME).add(BulkParameters.INITIATOR)
          .add(BulkParameters.DATE_ADDED).add(BulkParameters.STATUS).add(BulkParameters.ASSIGNEE)
          .add(BulkParameters.ASSIGNEE_DATE_ADDED)
          .build();

  private static final List<String> HEADER_LIST_WITH_C1_DETAILS =
      List.of(BulkParameters.PRODUCT_CODE, BulkParameters.PRODUCT_NAME, BulkParameters.CATEGORY_C1,
          BulkParameters.CATEGORY_CN, BulkParameters.STORE_NAME, BulkParameters.INITIATOR,
          BulkParameters.DATE_ADDED, BulkParameters.STATUS, BulkParameters.ASSIGNEE,
          BulkParameters.ASSIGNEE_DATE_ADDED);

  @Value("${add.c1.details.to.vendor.bulk.download}")
  private boolean addC1DetailsToVendorBulkDownload;

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) {
    List<String> headerList = new ArrayList<>(HEADER_LIST);
    if(addC1DetailsToVendorBulkDownload){
      headerList = new ArrayList<>(HEADER_LIST_WITH_C1_DETAILS);
    }
    return headerList;
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkVendorSummaryResponse bulkVendorSummaryResponse = (BulkVendorSummaryResponse) response;
    List<List<String>> rowData = new ArrayList<>();
    for (DistributionProductResponse distributionProduct : bulkVendorSummaryResponse.getResponseList()) {
      List<String> row = generateRow(distributionProduct);
      rowData.add(row);
    }
    return rowData;
  }

  private List<String> generateRow(DistributionProductResponse source) {
    List<String> row = new ArrayList<>();
    row.add(source.getProductCode());
    row.add(source.getProductName());
    if(addC1DetailsToVendorBulkDownload){
      row.add(source.getC1CategoryName());
    }
    row.add(source.getCategoryName());
    row.add(source.getBusinessPartnerName());
    row.add(source.getCreatedBy());
    row.add(convertDate(source.getCreatedDate()));
    if (source.isProductApproved())
      row.add(FINISHED);
    else
      row.add(NOT_FINISIHED);
    row.add(source.getProductApproverAssignee());
    row.add(convertDate(source.getProductAssignedDate()));
    return row;
  }

  private String convertDate(Date date) {
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat(Constant.DATE_FORMAT);
    try {
      return simpleDateFormat.format(date);
    } catch(Exception e) {
      return null;
    }
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.VENDOR_FILTERED_PRODUCT) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(request.getUsername()));
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM,
        MessageUtil.getMessage(EmailConstants.BULK_VENDOR_FILTERED_PRODUCT_DOWNLOAD_TEMPLATE_ID, lang));
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, EmailConstants.VENDOR_PRODUCT_SUBJECT);
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BulkVendorSummaryResponse bulkVendorSummaryResponse = (BulkVendorSummaryResponse) response;
    return bulkVendorSummaryResponse.getResponseList().size();
  }
}
