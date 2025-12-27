package com.gdn.mta.bulk.helper;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.RecatFailedProducts;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.RecatFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.RecatFailedProductResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;

@Component(value = "bulkRecatFailedProductsProcessHelper")
public class BulkRecatFailedProductsProcessHelper extends BulkProcessHelper{

  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.PRODUCT_CODE_EN).add(BulkParameters.PRODUCT_NAME_EN)
          .add(BulkParameters.MASTER_CATEGORY_CODE).add(BulkParameters.MASTER_CATEGORY_NAME)
          .add(BulkParameters.NEW_MASTER_CATEGORY_CODE).add(BulkParameters.NEW_MASTER_CATEGORY_NAME)
          .add(BulkParameters.ERROR_HEADER).build();

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
    RecatFailedProductResponse recatFailedProductResponse = (RecatFailedProductResponse) response;
    List<List<String>> rowData = new ArrayList<>();
    for( RecatFailedProducts recatFailedProducts : recatFailedProductResponse.getResponseList()){
      List<String> rowDatum = new ArrayList<>();
      rowDatum.add(recatFailedProducts.getProductCode());
      rowDatum.add(recatFailedProducts.getProductName());
      rowDatum.add(recatFailedProducts.getMasterCategoryCode());
      rowDatum.add(recatFailedProducts.getMasterCategoryName());
      rowDatum.add(recatFailedProducts.getNewMasterCategoryCode());
      rowDatum.add(recatFailedProducts.getNewMasterCategoryName());
      rowDatum.add(recatFailedProducts.getErrorMessage());
      rowData.add(rowDatum);
    }
    return rowData;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    RecatFailedProductsDownloadRequest recatFailedProductsDownloadRequest =
        (RecatFailedProductsDownloadRequest) request;
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.RECAT_FAILED_PRODUCTS) +
        recatFailedProductsDownloadRequest.getRecatRequestCode();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    return null;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    RecatFailedProductResponse recatFailedProductResponse =
        (RecatFailedProductResponse) response;
    return recatFailedProductResponse.getResponseList().size();
  }
}
