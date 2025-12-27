package com.gdn.mta.bulk.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletionData;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.models.BulkPriceUpdateRequestData;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.RichTextString;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellUtil;
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.stereotype.Component;

import com.gdn.partners.bulk.util.BulkConfigurationUpdateParameters;
import com.gdn.partners.bulk.util.BulkProductSuspensionParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.model.vo.UnmappedSkuResponse;

/**
 * Created by virajjasani on 27/07/16.
 */
@Component
@Slf4j
public class POIUtil {

  public static final List<String> SUSPENSION_HEADERS = Arrays
      .asList(BulkProductSuspensionParameters.SELLER_CODE, BulkProductSuspensionParameters.SELLER_NAME,
          BulkProductSuspensionParameters.PRODUCT_CODE, BulkProductSuspensionParameters.PRODUCT_NAME,
          BulkProductSuspensionParameters.REASON, BulkProductSuspensionParameters.SELLER_REASON_DESCRIPTION,
          BulkProductSuspensionParameters.FAILURE_REASON);

  public static final List<String> MERCHANT_CONFIG_HEADERS = Arrays
      .asList(BulkConfigurationUpdateParameters.SELLER_CODE, BulkConfigurationUpdateParameters.SELLER_NAME,
          BulkConfigurationUpdateParameters.REVIEW_CONFIG, BulkConfigurationUpdateParameters.FAILURE_REASON);

  public static final List<String> CATEGORY_CONFIG_HEADERS = Arrays
      .asList(BulkConfigurationUpdateParameters.CATEGORY_CODE, BulkConfigurationUpdateParameters.CATEGORY_NAME,
          BulkConfigurationUpdateParameters.REVIEW_CONFIG, BulkConfigurationUpdateParameters.FAILURE_REASON);
  public static final String PRODUCT_CODE = "Product Code";
  public static final String PRODUCT_NAME = "Product Name";
  public static final String DELETED_PRODUCTS = "Deleted Products";
  public static final String PRODUCT_NAME_FOR_DATA = "productName";

  public static Sheet getSheetForInputStream(InputStream inputStream, boolean isXlsxFormat,
      int sheetIndex) throws IOException {
    Sheet sheet = null;
    if (isXlsxFormat) {
      XSSFWorkbook workBook = new XSSFWorkbook(inputStream);
      sheet = workBook.getSheetAt(sheetIndex);
    } else {
      POIFSFileSystem fileSystem = new POIFSFileSystem(inputStream);
      Workbook workBook = new HSSFWorkbook(fileSystem);
      sheet = workBook.getSheetAt(sheetIndex);
    }
    return sheet;
  }

  public static Map<Integer, String> readHeadersFromExcel(Sheet sheet, int rowId) throws IOException {
    Map<Integer, String> headers = new TreeMap<>();
    Row headerRow = sheet.getRow(rowId);
    if (headerRow == null) {
      return new HashMap<>();
    }
    Iterator<Cell> cells = headerRow.cellIterator();
    int count = 0;
    while (cells.hasNext()) {
      Cell cell = cells.next();
      headers.put(count, cell.getStringCellValue());
      count++;
    }
    return headers;
  }

  public static Map<Integer, String> readHeadersFromCncExcel(Sheet sheet) throws IOException {
    Map<Integer, String> headers = new TreeMap<>();
    Row headerRow = sheet.getRow(0);
    if (headerRow == null) {
      return new HashMap<>();
    }
    Iterator<Cell> cells = headerRow.cellIterator();
    int count = 0;
    while (cells.hasNext()) {
      Cell cell = cells.next();
      if (BulkParameters.PRODUCT_NAME_NON_EDITABLE.equalsIgnoreCase(cell.getStringCellValue()) ||
          BulkParameters.PICKUP_POINT_NAME_NON_EDITABLE.equalsIgnoreCase(cell.getStringCellValue())) {
        continue;
      }
      headers.put(count, cell.getStringCellValue());
      count++;
    }
    return headers;
  }

  public static List<Map<String, String>> readFromExcelForBulkUpdate(Sheet sheet, int startFrom, int headerRowId,
      int offsetCellsFromLast, Map<Integer, String> updatedHeaders) {
    List<Map<String, String>> datas = new ArrayList<>();
    Map<Integer, String> headers = new TreeMap<>();
    Row headerRow = sheet.getRow(headerRowId);
    if (MapUtils.isEmpty(updatedHeaders)) {
      setHeaderValues(headerRow, headers);
    } else {
      headers = updatedHeaders;
    }
    Iterator<Row> rows = sheet.rowIterator();
    int numOfCell = headerRow.getPhysicalNumberOfCells() - offsetCellsFromLast;
    int counter = 0;
    while (rows.hasNext()) {
      Row row = rows.next();
      if (counter >= startFrom) {
        Map<String, String> data = new TreeMap<>();
        extractRowAndUpdateData(numOfCell, row, data, headers);
        if (!data.isEmpty()) {
          data.put("RowNumber", String.valueOf(row.getRowNum()));
          datas.add(data);
        }
      }
      counter++;
    }
    return datas;
  }

  public static List<Map<String, String>> readStringValueFromExcelForBulkUpdate(Sheet sheet, int startFrom,
      int headerRowId, int offsetCellsFromLast, String processType) {
    List<Map<String, String>> datas = new ArrayList<>();
    Map<Integer, String> headers = new TreeMap<>();
    Row headerRow = sheet.getRow(headerRowId);
    setHeaderValues(headerRow, headers);
    Iterator<Row> rows = sheet.rowIterator();
    int numOfCell = headerRow.getPhysicalNumberOfCells() - offsetCellsFromLast;
    int counter = 0;
    while (rows.hasNext()) {
      Row row = rows.next();
      if (counter >= startFrom) {
        Map<String, String> data = createRowMap(processType);
        extractStringRowData(numOfCell, row, data, headers);
        if (!data.isEmpty()) {
          data.put("RowNumber", String.valueOf(row.getRowNum()));
          datas.add(data);
        }
      }
      counter++;
    }
    return datas;
  }

  private static Map<String, String> createRowMap(String processType) {
    return BulkInternalProcessType.RECATEGORISATION.getValue().equalsIgnoreCase(processType)
      ? new LinkedHashMap<>()
      : new TreeMap<>();
  }

  public static List<Map<String, String>> readFromExcelFromSpecificHeadersForBulkPriceUpdate(Sheet sheet, int startFrom,
      int headerRowId, List<String> mandatoryHeaders) {
    List<Map<String, String>> datas = new ArrayList<>();
    Map<Integer, String> headerIndexToNameMap = new TreeMap<>();
    Row headerRow = sheet.getRow(headerRowId);
    setMandatoryHeaderValues(headerRow, headerIndexToNameMap, mandatoryHeaders);
    Iterator<Row> rows = sheet.rowIterator();
    int counter = 0;
    while (rows.hasNext()) {
      Row row = rows.next();
      if (counter >= startFrom) {
        Map<String, String> data = new TreeMap<>();
        extractStringRowDataFromMandatoryHeaders(row, data, headerIndexToNameMap);
        if (!data.isEmpty()) {
          data.put("RowNumber", String.valueOf(row.getRowNum()));
          datas.add(data);
        }
      }
      counter++;
    }
    return datas;
  }

  private static void setMandatoryHeaderValues(Row headerRow, Map<Integer, String> headers, List<String> mandatoryHeaders) {
    Iterator<Cell> cells = headerRow.cellIterator();
    int count = 0;
    while (cells.hasNext()) {
      Cell cell = cells.next();
      if (mandatoryHeaders.contains(cell.getStringCellValue())) {
        headers.put(count, cell.getStringCellValue());
      }
      count++;
    }
  }

  private static void extractStringRowDataFromMandatoryHeaders(Row row, Map<String, String> data, Map<Integer, String> headers) {
    Set<String> mandatoryFieldHeaders = BulkParameters.BULK_PRICE_UPDATE_NEW_MANDATORY_FIELD_HEADERS;
    boolean hasNonEmptyMandatoryField = false;
    for (int i : headers.keySet()) {
      Cell cell = row.getCell(i, Row.MissingCellPolicy.CREATE_NULL_AS_BLANK);
      if (cell.getCellType() != Cell.CELL_TYPE_STRING) {
        cell.setCellType(Cell.CELL_TYPE_STRING);
      }
      data.put(headers.get(i), cell.getStringCellValue());
      if (mandatoryFieldHeaders.contains(headers.get(i)) && !StringUtils.isEmpty(data.get(headers.get(i)).trim())) {
        hasNonEmptyMandatoryField = true; // At least one mandatory field has data
      }
    }
    if (!hasNonEmptyMandatoryField) {
      data.clear();
    }
  }

  public static List<Map<String, String>> readFromExcelForBulkUpdateWithCustomHeaders(Sheet sheet, int startFrom,
      Map<Integer, String> headers) {
    List<Map<String, String>> datas = new ArrayList<>();
    Iterator<Row> rows = sheet.rowIterator();
    int numOfCell = headers.size();
    int counter = 0;
    while (rows.hasNext()) {
      Row row = rows.next();
      if (counter >= startFrom) {
        Map<String, String> data = new TreeMap<>();
        extractRowAndUpdateData(numOfCell, row, data, headers);
        if (!data.isEmpty()) {
          data.put("RowNumber", String.valueOf(row.getRowNum()));
          datas.add(data);
        }
      }
      counter++;
    }
    return datas;
  }

  private static void setHeaderValues(Row headerRow, Map<Integer, String> headers) {
    Iterator<Cell> cells = headerRow.cellIterator();
    int count = 0;
    while (cells.hasNext()) {
      Cell cell = cells.next();
      headers.put(count, cell.getStringCellValue());
      count++;
    }
  }

  private static void extractRowAndUpdateData(int numOfCell, Row row, Map<String, String> data,
      Map<Integer, String> headers) {
    int emptyStringCount = 0;
    for (int i = 0; i < numOfCell; i++) {
      Cell cell = row.getCell(i, Row.MissingCellPolicy.CREATE_NULL_AS_BLANK);
      switch (cell.getCellType()) {
        case Cell.CELL_TYPE_NUMERIC: {
          data.put(headers.get(i), String.valueOf(cell.getNumericCellValue()));
          break;
        }
        case Cell.CELL_TYPE_STRING: {
          RichTextString richTextString = cell.getRichStringCellValue();
          data.put(headers.get(i), richTextString.getString());
          break;
        }
        default: {
          cell.setCellType(Cell.CELL_TYPE_STRING);
          data.put(headers.get(i), cell.getStringCellValue());
          break;
        }
      }
      if (StringUtils.isEmpty(data.get(headers.get(i)).trim())) {
        emptyStringCount++;
      }
    }
    if(emptyStringCount == numOfCell){
      data.clear();
    }
  }

  private static void extractStringRowData(int numOfCell, Row row, Map<String, String> data,
      Map<Integer, String> headers) {
    int emptyStringCount = 0;
    for (int i = 0; i < numOfCell; i++) {
      Cell cell = row.getCell(i, Row.MissingCellPolicy.CREATE_NULL_AS_BLANK);
      if (cell.getCellType() != Cell.CELL_TYPE_STRING) {
        cell.setCellType(Cell.CELL_TYPE_STRING);
      }
      data.put(headers.get(i), cell.getStringCellValue());
      if (StringUtils.isEmpty(data.get(headers.get(i)).trim())) {
        emptyStringCount++;
      }
    }
    if (emptyStringCount == numOfCell) {
      data.clear();
    }
  }

  public static List<Map<String, String>> readFromExcelForBulkUpdateHavingSellerSku(Sheet sheet, int startFrom,
      int headerRowId, int offsetCellsFromLast, boolean cncForWarehouseFeatureSwitch, ProfileResponse profileResponse,
      boolean bulkBasicInfoProcess) {
    List<Map<String, String>> datas = new ArrayList<>();
    Map<Integer, String> headers = new TreeMap<>();
    Row headerRow = sheet.getRow(headerRowId);
    setHeaderValues(headerRow, headers);
    Iterator<Row> rows = sheet.rowIterator();
    int numOfCell = headerRow.getPhysicalNumberOfCells() - offsetCellsFromLast;
    int counter = 0;
    while (rows.hasNext()) {
      Row row = rows.next();
      if (bulkBasicInfoProcess && counter >= startFrom && isFirstCellBlank(row)) {
        counter++;
        continue;
      }
      if (counter >= startFrom) {
        Map<String, String> data = new TreeMap<>();
        extractRowAndUpdateDataForBulkUpdate(numOfCell, row, data, headers, cncForWarehouseFeatureSwitch, profileResponse);
        if (!data.isEmpty()) {
          data.put("RowNumber", String.valueOf(row.getRowNum()));
          datas.add(data);
        }
      }
      counter++;
    }
    return datas;
  }

  private static boolean isFirstCellBlank(Row row) {
    Cell cell = row.getCell(0, Row.MissingCellPolicy.CREATE_NULL_AS_BLANK);
    if (cell == null) return true;

    CellType type = cell.getCellTypeEnum();
    return type == CellType.BLANK || (type == CellType.STRING && StringUtils.isBlank(cell.getStringCellValue()));
  }

  private static void extractRowAndUpdateDataForBulkUpdate(int numOfCell, Row row, Map<String, String> data,
      Map<Integer, String> headers, boolean cncForWarehouseFeatureSwitch, ProfileResponse profileResponse) {
    int emptyStringCount = 0;
    for (int i = 0; i < numOfCell; i++) {
      Cell cell = row.getCell(i, Row.MissingCellPolicy.CREATE_NULL_AS_BLANK);
      if (Constant.SELLER_SKU.equalsIgnoreCase(headers.get(i))) {
        if (!CellType.STRING.equals(cell.getCellTypeEnum())) {
          cell.setCellType(Cell.CELL_TYPE_STRING);
        }
          data.put(headers.get(i), cell.getStringCellValue());
      } else if (BulkParameters.AMPHI_SKU_STATUS.equalsIgnoreCase(headers.get(i))
          || BulkParameters.EXTERNAL_SKU_STATUS.equalsIgnoreCase(headers.get(i))
          || BulkParameters.CNC_STATUS_HEADER.equalsIgnoreCase(headers.get(i))
          || checkHeaderForCnc1P(cncForWarehouseFeatureSwitch, headers, i, profileResponse)) {
        if (Cell.CELL_TYPE_NUMERIC == cell.getCellType()) {
          data.put(headers.get(i), String.valueOf(cell.getNumericCellValue()));
        } else {
          if (StringUtils.isBlank(cell.getStringCellValue())) {
            data.put(headers.get(i), StringUtils.EMPTY);
          } else {
            data.put(headers.get(i), new Double(cell.getStringCellValue()).toString());
          }
        }
      } else {
        switch (cell.getCellType()) {
          case Cell.CELL_TYPE_NUMERIC: {
            data.put(headers.get(i), String.valueOf(cell.getNumericCellValue()));
            break;
          }
          case Cell.CELL_TYPE_STRING: {
            RichTextString richTextString = cell.getRichStringCellValue();
            data.put(headers.get(i), richTextString.getString());
            break;
          }
          default: {
            cell.setCellType(Cell.CELL_TYPE_STRING);
            data.put(headers.get(i), cell.getStringCellValue());
            break;
          }
        }
      }
      if (StringUtils.isEmpty(data.get(headers.get(i)).trim())) {
        emptyStringCount++;
      }
    }
    if (emptyStringCount == numOfCell) {
      data.clear();
    }
  }

  private static boolean checkHeaderForCnc1P(boolean cncForWarehouseFeatureSwitch,
      Map<Integer, String> headers, int i, ProfileResponse profileResponse) {
    if (!cncForWarehouseFeatureSwitch) {
      return false;
    }
    if (profileResponse.getCompany().isCncActivated()) {
      return BulkParameters.DELIVERY_STATUS_HEADER.equalsIgnoreCase(headers.get(i))
          || BulkParameters.AMPHI_SKU_STATUS_CNC_1P.equalsIgnoreCase(headers.get(i))
          || BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P.equalsIgnoreCase(headers.get(i));
    } else {
      return BulkParameters.AMPHI_SKU_STATUS_CNC_1P.equalsIgnoreCase(headers.get(i))
          || BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P.equalsIgnoreCase(headers.get(i));
    }
  }

  public SXSSFWorkbook generateXLFile(List<String> header, List<List<String>> productData,
      List<PickupPointDTO> pickupPoints) throws Exception {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet(BulkParameters.DATA_SHEET);
    Sheet pickupPointSheet = workbook.createSheet(BulkParameters.PICKUP_POINT_SHEET);
    int rowindex = 0;
    Row row;
    for (PickupPointDTO pickupPoint : pickupPoints) {
      row = pickupPointSheet.createRow((short) rowindex);
      rowindex++;
      Cell cell = row.createCell((short) 0);
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(pickupPoint.getCode());
      cell = row.createCell((short) 1);
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(pickupPoint.getName());
    }
    row = dataSheet.createRow((short) 0);
    int cellIndex = 0;
    for (String headers : header) {
      Cell cell = row.createCell((short) cellIndex);
      cellIndex++;
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(headers);
    }
    int rowid = 1;
    for (List<String> rowData : productData) {
      row = dataSheet.createRow(rowid);
      rowid++;
      int cellid = 0;
      for (String cellValue : rowData) {
        Cell cell = row.createCell(cellid);
        cellid++;
        cell.setCellValue(cellValue);
      }
    }
    return workbook;
  }

  public SXSSFWorkbook generateXLFileForWholesale(List<String> header, List<List<String>> productData) {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet(BulkParameters.DATA_SHEET);
    Row row;
    row = dataSheet.createRow((short) 0);
    int cellIndex = 0;
    for (String headers : header) {
      Cell cell = row.createCell((short) cellIndex);
      cellIndex++;
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(headers);
    }
    int rowid = 1;
    for (List<String> rowData : productData) {
      row = dataSheet.createRow(rowid);
      rowid++;
      int cellid = 0;
      for (String cellValue : rowData) {
        Cell cell = row.createCell(cellid);
        cellid++;
        cell.setCellValue(cellValue);
      }
    }
    return workbook;
  }

  public static SXSSFWorkbook generateXLFileForSuspension(List<HashMap<String, String>> failureData) throws Exception {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet(BulkProductSuspensionParameters.PRODUCT_DATA);
    Row row = dataSheet.createRow((short) 0);

    int cellIndex = 0;
    for (String header : SUSPENSION_HEADERS) {
      Cell cell = row.createCell((short) cellIndex);
      cellIndex++;
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(header);
    }

    int rowid = 1;
    for (Map<String, String> rowData : failureData) {
      row = dataSheet.createRow(rowid);
      rowid++;
      int cellid = 0;
      for (String cellValue : SUSPENSION_HEADERS) {
        Cell cell = row.createCell(cellid);
        cellid++;
        cell.setCellValue(rowData.get(cellValue));
      }
    }
    return workbook;
  }

  public static byte[] getByteContentFromExcel(Sheet excelSheetData) throws IOException{

    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    try {
      excelSheetData.getWorkbook().write(bos);
    } finally {
      bos.close();
    }
    return bos.toByteArray();
  }

  public static SXSSFWorkbook generateXLFileForConfiguration(List<Map<String, String>> failureData, String type)
      throws Exception {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet(BulkConfigurationUpdateParameters.DATA);
    Row row = dataSheet.createRow((short) 0);
    List<String> headers = BulkConfigurationUpdateParameters.MERCHANT.equalsIgnoreCase(type) ?
        MERCHANT_CONFIG_HEADERS :
        CATEGORY_CONFIG_HEADERS;
    int cellIndex = 0;
    for (String header : headers) {
      Cell cell = row.createCell((short) cellIndex);
      cellIndex++;
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(header);
    }
    int rowid = 1;
    for (Map<String, String> rowData : failureData) {
      row = dataSheet.createRow(rowid++);
      int cellid = 0;
      for (String cellValue : headers) {
        Cell cell = row.createCell(cellid);
        cellid++;
        cell.setCellValue(rowData.get(cellValue));
      }
    }
    return workbook;
  }

  // when there are no unmapped products
  public Workbook createUnamappedProductSkusSheetWithoutData(String username, String requestId,
      String categoryName) throws Exception {
    Workbook workbook = getBasicDownloadUnmappedSkusWorkBook(username, categoryName);
    Sheet sheet = workbook.getSheet(Constant.UNMAPPED_PRODUCT);
    Row headerRow = sheet.createRow(3);
    Cell catalogCell = headerRow.createCell(0);
    catalogCell.setCellValue(Constant.NO_UNMAPPED_PRODUCT_MESSAGE);
    sheet.addMergedRegion(new CellRangeAddress(3, 3, 0, 3));
    catalogCell.setCellStyle(getHeaderStyle(workbook));
    return workbook;
  }

  // when there are unmapped products
  public  Workbook createUnmappedSkusSheetWithData(String username, String requestId, String categoryName,
      int maxCatCount, Map<String, List<String>> categoryNameHirearchy, List<UnmappedSkuResponse> unmappedSkuResponses)
      throws Exception {
    Workbook workbook = getBasicDownloadUnmappedSkusWorkBook(username, categoryName);
    Sheet sheet = workbook.getSheet(Constant.UNMAPPED_PRODUCT);
    CellStyle headerStyle = getHeaderStyle(workbook);

    Row headerRow = sheet.createRow(3);
    Cell catalogCell = headerRow.createCell(0);
    catalogCell.setCellStyle(headerStyle);
    catalogCell.setCellValue(Constant.CATALOG_CODE);
    sheet.autoSizeColumn(0, true);

    for (int count = 1; count <= maxCatCount; count++) {
      Cell catHeaderCell = headerRow.createCell(count);
      catHeaderCell.setCellStyle(headerStyle);
      catHeaderCell.setCellValue(Constant.CATEGORY + count);
      sheet.autoSizeColumn(count, true);
    }

    Cell nameHeaderCell = headerRow.createCell(maxCatCount + 1);
    nameHeaderCell.setCellStyle(headerStyle);
    nameHeaderCell.setCellValue(Constant.PRODUCT_NAME);
    sheet.autoSizeColumn(maxCatCount + 1, true);

    Cell skuHeaderCell = headerRow.createCell(maxCatCount + 2);
    skuHeaderCell.setCellStyle(headerStyle);
    skuHeaderCell.setCellValue(Constant.SKU);
    sheet.autoSizeColumn(maxCatCount + 2, true);

    Cell createdDateHeaderCell = headerRow.createCell(maxCatCount + 3);
    createdDateHeaderCell.setCellStyle(headerStyle);
    createdDateHeaderCell.setCellValue(Constant.PRODUCT_CREATION_DATE);
    sheet.autoSizeColumn(maxCatCount + 3, true);

    CellStyle colStyle = getColumnStyle(workbook);
    int rowTracker = 4;
    for (UnmappedSkuResponse unmappedSkuResponse : unmappedSkuResponses) {
      Row row = sheet.createRow(rowTracker);

      Cell catalogCodeCell = row.createCell(0);
      catalogCodeCell.setCellStyle(colStyle);
      catalogCodeCell.setCellValue(unmappedSkuResponse.getMasterCatalogCode());

      List<String> sortedCategories = categoryNameHirearchy.get(unmappedSkuResponse.getMasterCategoryCode());
      for (int catCol = 1; catCol <= sortedCategories.size(); catCol++) {
        Cell categoryCell = row.createCell(catCol);
        categoryCell.setCellStyle(colStyle);
        categoryCell.setCellValue(sortedCategories.get(sortedCategories.size() - catCol));
      }

      Cell nameCell = row.createCell(maxCatCount + 1);
      nameCell.setCellStyle(colStyle);
      nameCell.setCellValue(unmappedSkuResponse.getProductName());

      Cell skuCell = row.createCell(maxCatCount + 2);
      skuCell.setCellStyle(colStyle);
      skuCell.setCellValue(unmappedSkuResponse.getProductSku());

      Cell createdDateCell = row.createCell(maxCatCount + 3);
      createdDateCell.setCellStyle(colStyle);
      createdDateCell.setCellValue(unmappedSkuResponse.getCreatedDate().toLocaleString());

      rowTracker ++;
    }

    return workbook;
  }

  // get basic template
  private  Workbook getBasicDownloadUnmappedSkusWorkBook(String username, String categoryName) {
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet(Constant.UNMAPPED_PRODUCT);
    getColumnStyle(workbook);
    Row picRow = sheet.createRow(0);
    Cell picCell = picRow.createCell(0);
    LocalDateTime date = LocalDateTime.now();
    picCell.setCellValue(
        String.format(Constant.GENERATED_DATE, username, date.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)));
    sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 6));
    Row listDateInfoRow = sheet.createRow(1);
    Cell listDateInfoCell = listDateInfoRow.createCell(0);
    listDateInfoCell.setCellValue(Constant.UNMAPPED_PRODUCTS_CREATED_IN + categoryName);
    sheet.addMergedRegion(new CellRangeAddress(1, 1, 0, 6));
    picCell.setCellStyle(getHeaderStyle(workbook));
    return workbook;
  }

  // get coloumn style
  private CellStyle getColumnStyle(Workbook workbook) {
    Font categoryFont = workbook.createFont();
    categoryFont.setBold(true);
    categoryFont.setFontHeightInPoints(XSSFFont.DEFAULT_FONT_SIZE);

    CellStyle colStyle = workbook.createCellStyle();
    colStyle.setVerticalAlignment(VerticalAlignment.CENTER);
    colStyle.setAlignment(HorizontalAlignment.CENTER);
    colStyle.setFont(categoryFont);
    colStyle.setBorderBottom(BorderStyle.THIN);
    colStyle.setBorderRight(BorderStyle.THIN);
    colStyle.setBorderLeft(BorderStyle.THIN);

    return colStyle;
  }

  // get header style
  private CellStyle getHeaderStyle(Workbook workbook) {
    Font headerFont = workbook.createFont();
    headerFont.setBold(true);
    headerFont.setFontHeightInPoints((short) 14);
    headerFont.setFontName(XSSFFont.DEFAULT_FONT_NAME);

    CellStyle headerStyle = workbook.createCellStyle();
    headerStyle.setFont(headerFont);
    headerStyle.setBorderBottom(BorderStyle.THICK);
    headerStyle.setBorderTop(BorderStyle.THICK);
    headerStyle.setBorderRight(BorderStyle.THICK);
    headerStyle.setBorderLeft(BorderStyle.THICK);

    return headerStyle;
  }


  public static void validateNumberOfRows(Sheet sheet, String bulkProcessCode, int numberOfRowsAllowed, boolean validateNumberOfRows) {
    if (validateNumberOfRows) {
      int lastRowNumber = sheet.getLastRowNum();
      log.info("Number of rows for bulk process : {} , numberOfRows : {} ", bulkProcessCode, lastRowNumber);
      if (lastRowNumber > numberOfRowsAllowed) {
        log.error("Number of rows is greater than threshold for process : {} , numberOfRows : {} , maxNumberOfRows : {} ", bulkProcessCode, lastRowNumber, numberOfRowsAllowed);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN);
      }
    }
  }

  public static SXSSFWorkbook generateXLFileForBulkPriceUpdate(List<String> headers,
      List<Pair<BulkPriceUpdateRequestData, String>> productData) {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    SXSSFSheet dataSheet = workbook.createSheet(BulkParameters.DATA_SHEET);
    dataSheet.trackAllColumnsForAutoSizing();
    Row row;
    row = dataSheet.createRow((short) 0);
    int cellIndex = 0;
    Font headerFont = workbook.createFont();
    headerFont.setBold(true);
    headerFont.setFontHeightInPoints((short) 14);
    headerFont.setFontName(XSSFFont.DEFAULT_FONT_NAME);

    CellStyle headerStyle = workbook.createCellStyle();
    headerStyle.setFont(headerFont);

    CellStyle errorCellStyle = workbook.createCellStyle();
    errorCellStyle.setWrapText(true);

    for (String header : headers) {
      Cell cell = row.createCell((short) cellIndex);
      cellIndex++;
      cell.setCellStyle(headerStyle);
      cell.setCellType(CellType.STRING);
      cell.setCellValue(header);
    }
    Cell rsultCell = row.createCell((short) cellIndex);
    rsultCell.setCellStyle(headerStyle);
    rsultCell.setCellType(CellType.STRING);
    rsultCell.setCellValue(BulkParameters.BULK_PRICE_UPDATE_RESULT);

    int rowid = 1;
    for (Pair<BulkPriceUpdateRequestData, String> rowData : productData) {
      row = dataSheet.createRow(rowid);
      rowid++;
      int cellid = 0;
      Cell cell;

      // ItemSku
      cell = row.createCell(cellid);
      cell.setCellValue(rowData.getLeft().getItemSku());
      cellid++;

      // Pickup Point Code
      cell = row.createCell(cellid);
      cell.setCellValue(rowData.getLeft().getPickupPointCode());
      cellid++;

      // Normal price
      cell = row.createCell(cellid);
      if (StringUtils.isNotEmpty(rowData.getLeft().getListPrice())) {
        cell.setCellValue(Double.parseDouble(rowData.getLeft().getListPrice()));
        CellUtil.setAlignment(cell, HorizontalAlignment.LEFT);
      } else {
        cell.setCellValue(StringUtils.EMPTY);
      }
      cellid++;

      // Selling price
      cell = row.createCell(cellid);
      if (StringUtils.isNotEmpty(rowData.getLeft().getSalesPrice())) {
        cell.setCellValue(Double.parseDouble(rowData.getLeft().getSalesPrice()));
        CellUtil.setAlignment(cell, HorizontalAlignment.LEFT);
      } else {
        cell.setCellValue(StringUtils.EMPTY);
      }
      cellid++;

      // Campaign code
      cell = row.createCell(cellid);
      cell.setCellValue(rowData.getLeft().getCampaignCode());
      cellid++;

      // Campaign price
      cell = row.createCell(cellid);
      if (StringUtils.isNotEmpty(rowData.getLeft().getCampaignPrice())) {
        cell.setCellValue(Double.parseDouble(rowData.getLeft().getCampaignPrice()));
        CellUtil.setAlignment(cell, HorizontalAlignment.LEFT);
      } else {
        cell.setCellValue(StringUtils.EMPTY);
      }
      cellid++;

      // Error message
      cell = row.createCell(cellid);
      cell.setCellStyle(errorCellStyle);
      cell.setCellValue(rowData.getRight());
    }
    for (int index = 0; index <= headers.size(); index++) {
      dataSheet.autoSizeColumn(index);
    }
    return workbook;
  }

  public static SXSSFWorkbook generateExcelFileForNotification(
      List<BulkNeedRevisionDeletionData> eligibleForDeletionList) throws JSONException {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet(DELETED_PRODUCTS);
    Font headerFont = workbook.createFont();
    headerFont.setBold(true);
    CellStyle headerStyle = workbook.createCellStyle();
    headerStyle.setFont(headerFont);
    Row headerRow = dataSheet.createRow(0);
    Cell headerCell0 = headerRow.createCell(0);
    headerCell0.setCellStyle(headerStyle);
    headerCell0.setCellValue(PRODUCT_CODE);
    Cell headerCell1 = headerRow.createCell(1);
    headerCell1.setCellValue(PRODUCT_NAME);
    headerCell1.setCellStyle(headerStyle);
    int rowNum = 1;
    for (BulkNeedRevisionDeletionData data : eligibleForDeletionList) {
      Row row = dataSheet.createRow(rowNum++);
      JSONObject jsonObject = new JSONObject(data.getProductData());
      row.createCell(0).setCellValue(data.getProductCode());
      row.createCell(1).setCellValue(jsonObject.getString(PRODUCT_NAME_FOR_DATA));
    }
    return workbook;
  }

  /**
   * Process Excel file to add error column and populate error messages
   */
  public static byte[] processExcelFileWithErrors(byte[] originalFileBytes,
    List<BulkProcessData> failedData, String genericSheetPassword, ObjectMapper objectMapper,
    boolean bundlingSeller)
    throws IOException {
    try (ByteArrayInputStream inputStream = new ByteArrayInputStream(originalFileBytes);
      Workbook workbook = new XSSFWorkbook(inputStream)) {

      // Access the "Upload Template" sheet
      Sheet dataSheet = workbook.getSheet(GenericBulkParameters.USER_INPUT_DATA_SHEET);

      // Find the last "Pilih value" column index
      int lastPilihValueColumnIndex = findLastPilihValueColumnIndex(dataSheet);

      // Add error column header after the last "Pilih value" column
      int errorColumnIndex = lastPilihValueColumnIndex + Constant.ONE;

      if (bundlingSeller) {
        errorColumnIndex = errorColumnIndex + Constant.TWO;
        dataSheet.setColumnHidden(errorColumnIndex, false);
      }

      // Shift cells to the right to make room for the ERROR column
      shiftCellsForErrorColumn(dataSheet, errorColumnIndex);

      // Populate error messages for failed rows
      populateErrorMessages(dataSheet, failedData, errorColumnIndex, objectMapper);

      // Auto-adjust column width based on content
      adjustErrorColumnWidth(dataSheet, errorColumnIndex, failedData);

      // Re-protect the sheet with the password "catalog"
      dataSheet.protectSheet(genericSheetPassword);

      // Convert workbook back to byte array
      try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
        workbook.write(outputStream);
        return outputStream.toByteArray();
      }
    }
  }

  /**
   * Find the last "Pilih value" column index in the header row
   */
  private static int findLastPilihValueColumnIndex(Sheet sheet) {
    // For Bahasa/Indonesian templates, headers are in row 11
    int headerRowIndex = 11;
    Row headerRow = sheet.getRow(headerRowIndex);

    int lastPilihValueIndex = -1;
    for (int i = 0; i < headerRow.getLastCellNum(); i++) {
      Cell cell = headerRow.getCell(i);
      if (cell != null && GenericBulkHeaders.ATTRIBUTE_VALUE.equals(cell.getStringCellValue())) {
        lastPilihValueIndex = i;
      }
    }

    return lastPilihValueIndex;
  }

  /**
   * Shift cells to the right to make room for the ERROR column
   */
  private static void shiftCellsForErrorColumn(Sheet sheet, int insertColumnIndex) {
    // Shift cells to the right and insert "ERROR" column
    for (int rowIndex = 0; rowIndex <= sheet.getLastRowNum(); rowIndex++) {
      Row row = sheet.getRow(rowIndex);
      if (Objects.isNull(row)) {
        continue;
      }

      // Shift cells to the right from the last cell down to insertColumnIndex
      for (int colIndex = row.getLastCellNum() - 1; colIndex >= insertColumnIndex; colIndex--) {
        Cell oldCell = row.getCell(colIndex);
        Cell newCell = row.getCell(colIndex + 1);
        if (Objects.isNull(newCell)) {
          newCell = row.createCell(colIndex + 1);
        }
        if (oldCell != null) {
          switch (oldCell.getCellTypeEnum()) {
            case STRING:
              newCell.setCellValue(oldCell.getStringCellValue());
              break;
            case NUMERIC:
              newCell.setCellValue(oldCell.getNumericCellValue());
              break;
            case BOOLEAN:
              newCell.setCellValue(oldCell.getBooleanCellValue());
              break;
            case FORMULA:
              newCell.setCellFormula(oldCell.getCellFormula());
              break;
            case BLANK:
              newCell.setCellType(CellType.BLANK);
              break;
            default:
              newCell.setCellValue(oldCell.toString());
          }
          newCell.setCellStyle(oldCell.getCellStyle());
        }
      }

      // Create new "ERROR" cell at insertColumnIndex
      Cell errorCell = row.getCell(insertColumnIndex);
      if (Objects.isNull(errorCell)) {
        errorCell = row.createCell(insertColumnIndex);
      }

      // Clear any existing content in the error column
      errorCell.setCellValue(org.apache.commons.lang.StringUtils.EMPTY);

      // Only set "Error" value in header row (row 11), other rows remain empty
      if (rowIndex == 11) { // Header row
        errorCell.setCellValue(GenericBulkHeaders.ERROR_COLUMN);
      }

      Cell referenceCell = row.getCell(insertColumnIndex - 1);
      if (Objects.nonNull(referenceCell)) {
        errorCell.setCellStyle(referenceCell.getCellStyle());
      }
    }
  }

  /**
   * Populate error messages for failed rows
   */
  private static void populateErrorMessages(Sheet sheet, List<BulkProcessData> failedData,
    int errorColumnIndex, ObjectMapper objectMapper) {
    Row headerRow = sheet.getRow(11);

    // Create a map of header names to column indices
    Map<String, Integer> headerColumnMap = new HashMap<>();
    for (int i = 0; i < headerRow.getLastCellNum(); i++) {
      Cell cell = headerRow.getCell(i);
      if (Objects.nonNull(cell) && StringUtils.isNotBlank(cell.getStringCellValue())) {
        headerColumnMap.put(cell.getStringCellValue().trim(), i);
      }
    }

    // Process each failed data record
    int dataRowIndex = 14;
    for (BulkProcessData data : failedData) {
      if (Objects.isNull(data.getRowNumber()) || StringUtils.isBlank(data.getBulkRequestData())) {
        continue;
      }

      try {
        // Get the data row for this specific failed record
        Row dataRow = sheet.getRow(dataRowIndex);
        if (Objects.isNull(dataRow)) {
          dataRow = sheet.createRow(dataRowIndex);
        }

        // Parse the JSON data from bulkRequestData
        Map<String, String> rowDataJson = objectMapper.readValue(data.getBulkRequestData(),
            new TypeReference<LinkedHashMap<String, String>>() {});

        // Populate data from JSON into the corresponding columns
        for (Map.Entry<String, String> entry : rowDataJson.entrySet()) {
          String headerName = entry.getKey();
          String value = entry.getValue();

          // Find the column index for this header
          Integer columnIndex = headerColumnMap.get(headerName);
          if (Objects.nonNull(columnIndex) && StringUtils.isNotBlank(value)) {
            Cell dataCell = dataRow.getCell(columnIndex);
            if (Objects.isNull(dataCell)) {
              dataCell = dataRow.createCell(columnIndex);
            }
            dataCell.setCellValue(value);

            // Special handling for "Kategori*" - populate C2 and CN in adjacent columns
            if (GenericBulkHeaders.CATEGORY.equals(headerName)) {
              // Populate C2 in the next column (colIndex + 1)
              String c2Value = rowDataJson.get(GenericBulkHeaders.C2);
              if (StringUtils.isNotBlank(c2Value)) {
                Cell c2Cell = dataRow.getCell(columnIndex + 1);
                if (Objects.isNull(c2Cell)) {
                  c2Cell = dataRow.createCell(columnIndex + 1);
                }
                c2Cell.setCellValue(c2Value);
              }

              // Populate CN in the column after that (colIndex + 2)
              String cnValue = rowDataJson.get(GenericBulkHeaders.CN);
              if (StringUtils.isNotBlank(cnValue)) {
                Cell cnCell = dataRow.getCell(columnIndex + 2);
                if (Objects.isNull(cnCell)) {
                  cnCell = dataRow.createCell(columnIndex + 2);
                }
                cnCell.setCellValue(cnValue);
              }
            }
          }
        }

        // Add error message to the ERROR column
        if (org.apache.commons.lang.StringUtils.isNotBlank(data.getErrorMessage())) {
          Cell errorCell = dataRow.getCell(errorColumnIndex);
          if (Objects.isNull(errorCell)) {
            errorCell = dataRow.createCell(errorColumnIndex);
          }
          errorCell.setCellValue(data.getErrorMessage());
        }

        // Move to next row for the next failed record
        dataRowIndex++;

      } catch (Exception e) {
        log.error("Error parsing bulkRequestData for row {}: {}", data.getRowNumber(),
          e.getMessage());
      }
    }
  }

  /**
   * Auto-adjust error column width based on content
   */
  private static void adjustErrorColumnWidth(Sheet sheet, int errorColumnIndex,
    List<BulkProcessData> failedData) {
    // Calculate the maximum error message length
    int maxErrorLength = 0;
    for (BulkProcessData data : failedData) {
      if (StringUtils.isNotEmpty(data.getErrorMessage())) {
        maxErrorLength = Math.max(maxErrorLength, data.getErrorMessage().length());
      }
    }

    // Add small buffer and cap at 200 to avoid Excel's 255 limit
    int columnWidth = Math.min(maxErrorLength + 2, 200);

    // Set column width (POI uses 256 units per character)
    sheet.setColumnWidth(errorColumnIndex, columnWidth * 256);
  }
}
