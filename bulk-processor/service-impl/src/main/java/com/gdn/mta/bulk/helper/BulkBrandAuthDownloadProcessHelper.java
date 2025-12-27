package com.gdn.mta.bulk.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BrandAuthDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BrandAuthResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;

@Service(value = "bulkBrandAuthDownloadProcessHelper")
public class BulkBrandAuthDownloadProcessHelper extends BulkProcessHelper {
  private static final String SELLER_ID = "Seller Code";
  private static final String SELLER_NAME = "Seller Name";
  private static final String BRAND_ID = "Brand Code";
  private static final String BRAND_NAME = "Brand Name";
  private static final String AUTHORIZATION_START_DATE = "Authorisation Start date ";
  private static final String AUTHORIZATION_END_DATE = "Authorisation End Date";


  private static final List<String> HEADER_LIST =
    ImmutableList.<String>builder().add(BRAND_ID).add(BRAND_NAME).add(SELLER_ID).add(SELLER_NAME)
      .add(AUTHORIZATION_START_DATE).add(AUTHORIZATION_END_DATE).build();

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
    BrandAuthDataResponse brandAuthDataResponse = (BrandAuthDataResponse) response;
    List<List<String>> rowData = new ArrayList<>();
    for (BrandAuthResponse brandAuthResponse : brandAuthDataResponse.getBrandAuthResponseList()) {
      List<String> row =
        Arrays.asList(brandAuthResponse.getBrandCode(), brandAuthResponse.getBrandName(),
          brandAuthResponse.getSellerCode(), brandAuthResponse.getSellerName(),
          brandAuthResponse.getAuthStartDate(), brandAuthResponse.getAuthEndDate());
      rowData.add(row);
    }
    return rowData;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.BRAND_AUTHORIZATION_DOWNLOAD) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(request.getUsername()));
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM, EmailConstants.BRAND_AUTH_DOWNLOAD_TEMPLATE_ID);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, EmailConstants.BRAND_AUTH_DOWNLOAD_TEMPLATE_SUBJECT);
    emailParameters.put(EmailConstants.FILE_PREFIX, ProcessorUtils.FILETYPE_XLSX_EXCEL);
    return emailParameters;

  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BrandAuthDataResponse brandAuthDataResponse = (BrandAuthDataResponse) response;
    return brandAuthDataResponse.getBrandAuthResponseList().size();
  }
}
