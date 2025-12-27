package com.gdn.mta.bulk.helper;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import com.gdn.partners.bulk.util.Constant;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductVendorResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;

@Component("bulkVendorProcessHelper")
public class BulkVendorProcessHelper extends BulkProcessHelper {

  private static final String FINISHED = "Selesai";
  private static final String NOT_FINISIHED = "Belum";

  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.PRODUCT_CODE)
          .add(BulkParameters.PRODUCT_NAME).add(BulkParameters.CATEGORY)
          .add(BulkParameters.STORE_NAME).add(BulkParameters.INITIATOR)
          .add(BulkParameters.DATE_ADDED).add(BulkParameters.CONTENT).add(BulkParameters.IMAGE)
          .build();

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) {
    List<String> headerList = new ArrayList<>(HEADER_LIST);
    return headerList;
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkProductVendorResponse productVendorResponse = (BulkProductVendorResponse) response;
    List<List<String>> rowData = new ArrayList<>();
    for (DistributionProductResponse distributionProduct : productVendorResponse.getResponseList()) {
      List<String> row = generateRow(distributionProduct);
      rowData.add(row);
    }
    return rowData;
  }

  private List<String> generateRow(DistributionProductResponse source) {
    List<String> row = new ArrayList<>();
    row.add(source.getProductCode());
    row.add(source.getProductName());
    row.add(source.getCategoryName());
    row.add(source.getBusinessPartnerName());
    row.add(source.getCreatedBy());
    row.add(convertDate(source.getCreatedDate()));
    if (source.isProductApproved())
      row.add(FINISHED);
    else
      row.add(NOT_FINISIHED);
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
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.PRODUCT_VENDOR) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    return null;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BulkProductVendorResponse productVendorResponse = (BulkProductVendorResponse) response;
    return productVendorResponse.getResponseList().size();
  }
}
