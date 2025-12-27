package com.gdn.mta.bulk.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterProductResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;

@Component(value = "bulkMasterProductProcessHelper")
public class BulkMasterProductProcessHelper extends BulkProcessHelper {
  @Autowired
  private FileStorageOperationsService fileStorageOperationsService;
  private static final int FIRST_ROW = 1;
  private static final int FIRST_COLUMN = 8;
  private static final int LAST_COLUMN = 8;
  private List<String> dGlevels =
      Arrays.asList(BulkParameters.DG_LEVEL1, BulkParameters.DG_LEVEL2, BulkParameters.DG_LEVEL3);
  private static final List<String> HEADER_LIST = Arrays
      .asList(BulkParameters.PRODUCT_CODE, BulkParameters.CATEGORY, BulkParameters.PRODUCT_NAME, BulkParameters.BRAND,
          BulkParameters.LENGTH, BulkParameters.WIDTH, BulkParameters.HEIGHT, BulkParameters.WEIGHT,
          BulkParameters.DG_LEVEL);

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) {
    return HEADER_LIST;
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    BulkMasterProductResponse bulkMasterProductResponse = (BulkMasterProductResponse) response;
    Sheet dgLevel = workbook.createSheet(BulkParameters.DG_LEVEL);
    int rowindex = 0;
    Row row;
    row = dgLevel.createRow(rowindex);
    rowindex++;
    Cell cell = row.createCell(0);
    cell.setCellValue(BulkParameters.DG_LEVEL);
    for (String dGoodsLevel : bulkMasterProductResponse.getdGLevels()) {
      row = dgLevel.createRow(rowindex);
      rowindex++;
      Cell cellValue = row.createCell((short) 0);
      cellValue.setCellValue(dGoodsLevel);
    }
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkMasterProductResponse productResponse = (BulkMasterProductResponse) response;
    return getContents(productResponse.getResponseList());
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.MASTER_PRODUCT) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(request.getUsername()));
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM,
        MessageUtil.getMessage(EmailConstants.PRODUCT_DOWNLOAD_TEMPLATE_ID, lang));
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, EmailConstants.MASTER_PRODUCT_SUBJECT);
    emailParameters.put(EmailConstants.FILE_PREFIX,
        fileStorageOperationsService.getFilePrefix(BulkProcessType.MASTER_PRODUCT.getValue()));
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BulkMasterProductResponse bulkMasterProductResponse = (BulkMasterProductResponse) response;
    return bulkMasterProductResponse.getResponseList().size();
  }

  @Override
  public Workbook generateDataSheet(List<String> headerList, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth) {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet(BulkParameters.DATA_SHEET);
    Row row;
    row = dataSheet.createRow((short) 0);
    int cellIndex = 0;
    for (String headers : headerList) {
      Cell cell = row.createCell((short) cellIndex);
      cellIndex++;
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(headers);
    }
    int rowid = 1;
    for (List<String> rowData : rowDataList) {
      row = dataSheet.createRow(rowid);
      rowid++;
      int cellid = 0;
      for (String cellValue : rowData) {
        Cell cell = row.createCell(cellid);
        cellid++;
        cell.setCellValue(cellValue);
      }
    }
    CellRangeAddressList cellRangeAddressList =
        new CellRangeAddressList(FIRST_ROW, rowDataList.size() + 1, FIRST_COLUMN, LAST_COLUMN);
    DataValidationHelper dataValidationHelper = dataSheet.getDataValidationHelper();
    DataValidationConstraint dataValidationConstraint =
        dataValidationHelper.createExplicitListConstraint(dGlevels.stream().toArray(String[]::new));
    DataValidation dataValidation =
        dataValidationHelper.createValidation(dataValidationConstraint, cellRangeAddressList);
    dataValidation.setSuppressDropDownArrow(true);
    dataValidation.setShowErrorBox(true);
    dataValidation.setShowPromptBox(true);
    dataSheet.addValidationData(dataValidation);
    return workbook;
  }

  private List<List<String>> getContents(List<MasterProductResponse> masterProductResponses) {
    List<List<String>> productContents = new ArrayList<>();
    masterProductResponses.forEach(masterProductResponse -> getProductContents(productContents, masterProductResponse));
    return productContents;
  }

  private void getProductContents(List<List<String>> productContents, MasterProductResponse masterProductResponse) {
    List<String> response = new ArrayList<>();
    response.add(masterProductResponse.getProductCode());
    response.add(masterProductResponse.getCategoryCode());
    response.add(masterProductResponse.getName());
    response.add(masterProductResponse.getBrand());
    response.add(String.valueOf(masterProductResponse.getLength()));
    response.add(String.valueOf(masterProductResponse.getWidth()));
    response.add(String.valueOf(masterProductResponse.getHeight()));
    response.add(String.valueOf(masterProductResponse.getWeight()));
    response.add(String.valueOf(masterProductResponse.getdGLevel()));
    productContents.add(response);
  }
}
