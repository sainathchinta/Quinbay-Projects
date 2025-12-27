package com.gdn.mta.bulk.helper;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.stereotype.Component;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkReviewProductResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;

/**
 * Created by govind on 29/01/2019 AD.
 */

@Component(value = "bulkReviewProductProcessHelper")
public class BulkReviewProductProcessHelper extends BulkProcessHelper {

  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.PRODUCT_CODE)
          .add(BulkParameters.PRODUCT_NAME).add(BulkParameters.BRAND)
          .add(BulkParameters.CATEGORY_CODE).add(BulkParameters.CATEGORY_NAME)
          .add(BulkParameters.BUSINESS_PARTNER_CODE).add(BulkParameters.BUSINESS_PARTNER_NAME)
          .add(BulkParameters.ASSIGNED_TO).add(BulkParameters.SUBMITTED_DATE)
          .add(BulkParameters.STATE).build();

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) {
    return HEADER_LIST;
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response)
      throws Exception {
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkReviewProductResponse productReviewResponse = (BulkReviewProductResponse) response;
    return getContents(productReviewResponse.getResponseList());
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.REVIEW_PRODUCTS) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(request.getUsername()));
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM,
        MessageUtil.getMessage(EmailConstants.BULK_REVIEW_PRODUCT_DOWNLOAD_TEMPLATE_ID, lang));
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, EmailConstants.REVIEW_PRODUCT_SUBJECT);
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BulkReviewProductResponse bulkReviewProductResponse = (BulkReviewProductResponse) response;
    return bulkReviewProductResponse.getResponseList().size();
  }

  @Override
  public Workbook generateDataSheet(List<String> headerList, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth)
    throws IOException {
    return super.generateDataSheet(headerList, rowDataList, bulkUpdateTemplateColumnWidth);
  }

  private List<List<String>> getContents(List<ReviewProductResponse> reviewProductResponses) {
    return reviewProductResponses.stream()
        .map(this :: getProductContents)
        .collect(Collectors.toList());
  }

  private List<String> getProductContents(ReviewProductResponse reviewProductResponse) {
    List<String> response = new ArrayList<>();
    response.add(reviewProductResponse.getProductCode());
    response.add(reviewProductResponse.getProductName());
    response.add(reviewProductResponse.getBrand());
    response.add(reviewProductResponse.getCategoryCode());
    response.add(reviewProductResponse.getCategoryName());
    response.add(reviewProductResponse.getBusinessPartnerCode());
    response.add(reviewProductResponse.getBusinessPartnerName());
    response.add(String.valueOf(reviewProductResponse.getAssignedTo()));
    response.add(String.valueOf(reviewProductResponse.getSubmittedDate()));
    response.add(String.valueOf(reviewProductResponse.getState()));
    return response;
  }
}
