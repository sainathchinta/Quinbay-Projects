package com.gdn.mta.bulk.helper;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.FormulaEvaluator;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductBasicInfoResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.ExcelHeaderNames;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class BulkProductBasicInfoProcessHelper extends BulkProcessHelper {

  public static final String BULK_BASIC_INFO_REGULAR_TEMPLATE = "BulkBasicInfoRegularTemplate.xlsx";
  public static final String BULK_BASIC_INFO_INSTORE_TEMPLATE = "BulkBasicInfoInstoreTemplate.xlsx";

  private static final String SHEET = "Sheet1";
  private static final int ROW_START_INDEX = 4;

  @Autowired
  private FileStorageOperationsService fileStorageOperationsService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) throws Exception {
    BulkProductBasicInfoResponse basicInfoResponse = (BulkProductBasicInfoResponse) bulkDataResponse;
    if (basicInfoResponse.isBusinessPartnerO2O()) {
      return BulkParameters.getBasicInfoHeaderList(true, false);
    } else {
      return BulkParameters.getBasicInfoHeaderList(false, false);
    }
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    List<String> shippingOptionsAvailable =
        getShippingTypeForNonInternationalMerchant(response.getShippingTypeEligibility());
    Sheet shippingTypeSheet = workbook.createSheet(BulkParameters.SHIPPING_TYPE);
    workbook.setSheetHidden(workbook.getSheetIndex(shippingTypeSheet), true);

    int rowIndex = 0;
    for (String shippingOption : shippingOptionsAvailable) {
      Row row = shippingTypeSheet.createRow(rowIndex++);
      Cell cell = row.createCell(0);
      cell.setCellValue(shippingOption);
    }

    Sheet sheet = workbook.getSheetAt(0);
    int shippingColumnIndex = findColumnIndex(sheet);

    if (shippingColumnIndex == -1) {
      throw new IllegalArgumentException("Shipping column not found in the sheet.");
    }

    Name namedRange = workbook.createName();
    namedRange.setNameName("ShippingOptions");
    namedRange.setRefersToFormula("'Tipe pengiriman'!$A$1:$A$" + shippingOptionsAvailable.size());
    SystemParameterConfig systemParameterConfig =
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_BASIC_INFO_MAXIMUM_SIZE);
    applyDropDownToColumn(sheet, "ShippingOptions", shippingColumnIndex, 4,
        Optional.ofNullable(systemParameterConfig)
            .map(config -> Integer.parseInt(config.getValue()) + 4).orElse(10004));
    return workbook;
  }

  public static List<String> getShippingTypeForNonInternationalMerchant(
      ShippingTypeEligibility shippingTypeEligibility) {
    List<String> shippingTypeList = new ArrayList<>();
    shippingTypeList.add(ExcelHeaderNames.SHIPPED_BY_BLIBLI_ID);
    if (shippingTypeEligibility.isEligibleForBigProduct()) {
      shippingTypeList.add(ExcelHeaderNames.SHIPPED_BY_SELLER_ID);
    }
    if (shippingTypeEligibility.isEligibleForBopisProduct()) {
      shippingTypeList.add(ExcelHeaderNames.BOPIS_ID);
    }
    return shippingTypeList;
  }

  private int findColumnIndex(Sheet sheet) {
    Row headerRow = sheet.getRow(0);
    if (headerRow == null)
      return -1;

    for (Cell cell : headerRow) {
      if (cell.getStringCellValue().trim().equalsIgnoreCase(BulkParameters.SHIPPING_TYPE)) {
        return cell.getColumnIndex();
      }
    }
    return -1;
  }

  private void applyDropDownToColumn(Sheet sheet, String namedRangeName, int columnIndex, int startRow, int endRow) {
    DataValidationHelper helper = sheet.getDataValidationHelper();
    DataValidationConstraint constraint = helper.createFormulaListConstraint(namedRangeName);
    CellRangeAddressList addressList = new CellRangeAddressList(startRow, endRow, columnIndex, columnIndex);
    DataValidation validation = helper.createValidation(constraint, addressList);
    validation.setSuppressDropDownArrow(true);
    validation.setShowErrorBox(true);
    sheet.addValidationData(validation);
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkProductBasicInfoResponse productResponse = (BulkProductBasicInfoResponse) response;
    return productResponse.getProductContentList();
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.PRODUCT_BASIC_INFO) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    return new HashMap<>();
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    if (Objects.nonNull(response) && response instanceof BulkProductBasicInfoResponse) {
      BulkProductBasicInfoResponse bulkProductBasicInfoResponse = (BulkProductBasicInfoResponse) response;
      return Optional.ofNullable(bulkProductBasicInfoResponse.getProductContentList()).map(List::size).orElse(0);
    }
    return 0;
  }

  @Override
  public Workbook generateDataSheet(List<String> headerList, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth) throws IOException {
    String fileName = determineTemplateFileName(headerList);
    XSSFWorkbook workbook = loadWorkbook(fileName);
    XSSFSheet dataSheet = getSheet(workbook);
    populateDataSheet(headerList, dataSheet, rowDataList);
    evaluateFormulas(workbook);
    return workbook;
  }

  private String determineTemplateFileName(List<String> headerList) {
    return headerList.contains(BulkParameters.INSTORE) ?
        BULK_BASIC_INFO_INSTORE_TEMPLATE :
        BULK_BASIC_INFO_REGULAR_TEMPLATE;
  }

  public XSSFWorkbook loadWorkbook(String fileName) throws IOException {
    try (InputStream inputStream = new ByteArrayInputStream(
        fileStorageOperationsService.downloadBaseTemplateForBulkBasicInfoUpdate(fileName))) {
      return new XSSFWorkbook(inputStream);
    } catch (Exception e) {
      log.error("Error while loading the workbook or template file: {}", fileName, e);
      throw e;
    }
  }

  private XSSFSheet getSheet(XSSFWorkbook workbook) {
    XSSFSheet sheet = workbook.getSheet(SHEET);
    if (Objects.isNull(sheet)) {
      throw new IllegalArgumentException("Sheet '" + SHEET + "' not found in the template.");
    }
    return sheet;
  }

  private void populateDataSheet(List<String> headerList, XSSFSheet dataSheet, List<List<String>> rowDataList) {
    int rowId = ROW_START_INDEX;
    for (List<String> rowData : rowDataList) {
      rowId = populateDataSheetRows(headerList, dataSheet, rowId, rowData);
    }
  }

  private void evaluateFormulas(Workbook workbook) {
    FormulaEvaluator evaluator = workbook.getCreationHelper().createFormulaEvaluator();
    evaluator.evaluateAll();
  }

  private static int populateDataSheetRows(List<String> headerList, XSSFSheet dataSheet, int rowId,
      List<String> rowData) {
    Row row = dataSheet.getRow(rowId);
    if (Objects.isNull(row)) {
      row = dataSheet.createRow(rowId);
    }
    rowId++;
    int cellIndex = 0;
    for (String cellValue : rowData) {
      cellIndex = setCellValueBasedOnHeader(headerList, row, cellIndex, cellValue);
    }
    return rowId;
  }

  private static int setCellValueBasedOnHeader(List<String> headerList, Row row, int cellIndex, String cellValue) {
    Cell cell = row.getCell(cellIndex);
    if (Objects.isNull(cell)) {
      cell = row.createCell(cellIndex);
    }
    if (BulkParameters.SHIPPING_HEADERS.contains(headerList.get(cellIndex))) {
      try {
        // Parse the value as a double and set it for shipping weight calculation in sheet
        cell.setCellValue(Double.parseDouble(cellValue));
      } catch (NumberFormatException e) {
        log.warn("Invalid double value for cell: {}", cellValue, e);
        cell.setCellValue(0.0); // Default to 0.0 or handle as needed
      }
    } else {
      cell.setCellValue(cellValue);
    }
    cellIndex++;
    return cellIndex;
  }
}
