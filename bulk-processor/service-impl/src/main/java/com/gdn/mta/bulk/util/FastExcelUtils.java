package com.gdn.mta.bulk.util;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.ValidationException;
import com.gdn.mta.bulk.models.PlatformConfig;
import com.gdn.mta.bulk.models.SheetConfig;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.dhatim.fastexcel.reader.Cell;
import org.dhatim.fastexcel.reader.ReadableWorkbook;
import org.dhatim.fastexcel.reader.Row;
import org.dhatim.fastexcel.reader.Sheet;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

@Slf4j
public class FastExcelUtils {

  public static final String ROW_NUMBER = "RowNumber";

  public static void validateHeadersAndMaxCols(SheetConfig config, String fileName, Row headerRow,
    String zipFileName) {
    if (headerRow == null) {
      throw new ValidationException(ErrorCategory.VALIDATION.getCode(),
        "Header " + fileName + " tidak ditemukan. Process - " + zipFileName);
    }
    // --- Validate max columns ---
    if (headerRow.getCellCount() > config.getMaxColumns()) {
      throw new ValidationException(ErrorCategory.VALIDATION.getCode(),
        String.format("Melebihi batas jumlah maksimum kolom untuk %s. Process - %s", fileName,
          zipFileName));
    }
  }

  public static Map<String, Integer> getHeaderIndexMap(Row headerRow) {
    Map<String, Integer> headerIndexMap = new HashMap<>();
    for (int i = 0; i < headerRow.getCellCount(); i++) {
      String cellText = headerRow.getCellText(i).trim();
      headerIndexMap.put(cellText.toLowerCase(), i);
    }
    return headerIndexMap;
  }

  public static void processEachRowPerSheet(String zipFileName, String fileName,
    Map<String, Boolean> sourceColumnToMandatoryMap,
    Map<String, StringBuilder> productValidationErrorMap, SheetConfig config,
    Iterator<Row> iterator, int currentRowNum, Integer joinKeyIndex, int validDataRows,
    Map<String, Integer> requiredColumnIndexMap, Map<String, List<Map<String, String>>> sheetData) {
    while (iterator.hasNext()) {
      Row row = iterator.next();
      currentRowNum++;
      String joinKeyValue = row.getCellText(joinKeyIndex);
      if (StringUtils.isBlank(joinKeyValue)) {
        log.warn("Skipping row {}: empty join key ", currentRowNum);
        continue;
      }
      validDataRows++;
      if (validDataRows > config.getMaxRows()) {
        throw new ValidationException(ErrorCategory.VALIDATION.getCode(),
          "Melebihi batas jumlah maksimum baris untuk " + fileName + ". Process - " + zipFileName);
      }

      Map<String, String> rowData = new HashMap<>();
      for (Map.Entry<String, Integer> entryCol : requiredColumnIndexMap.entrySet()) {
        String colName = entryCol.getKey();
        int colIndex = entryCol.getValue();
        String cellValue = StringUtils.trimToEmpty(row.getCellText(colIndex));
        // Mandatory validation
        if (sourceColumnToMandatoryMap.getOrDefault(colName, false) && StringUtils.isBlank(
          cellValue)) {
          productValidationErrorMap.computeIfAbsent(joinKeyValue, k -> new StringBuilder()).append(
            String.format("%s tidak ditemukan di : %s . Process - %s ", colName, fileName,
              zipFileName));
        }
        rowData.put(colName, cellValue);
      }
      rowData.put(ROW_NUMBER, String.valueOf(currentRowNum));
      sheetData.computeIfAbsent(joinKeyValue, k -> new ArrayList<>()).add(rowData);
    }
  }

  public static void processEachFileForExternalUpload(String zipFileName,
    Map<String, String> sourceColumnToDestinationColumnMap,
    Map<String, Boolean> sourceColumnToMandatoryMap,
    Map<String, StringBuilder> productValidationErrorMap,
    Map<String, List<Map<String, String>>> productBasicInfoMap,
    Map<String, List<Map<String, String>>> productMediaInfoMap,
    Map<String, List<Map<String, String>>> productShippingInfoMap,
    Map<String, List<Map<String, String>>> productSalesInfoMap, ReadableWorkbook workbook,
    SheetConfig config, String fileName, String joinKey, String sheetType) throws IOException {
    Optional<Sheet> sheetOptional = workbook.getSheet(config.getSheetIndex());
    Sheet sheet = sheetOptional.orElseThrow(
      () -> new ValidationException(ErrorCategory.VALIDATION.getCode(),
        "Data di sheet index : " + config.getSheetIndex() + " untuk " + fileName
          + " tidak ditemukan. " + "Process - " + zipFileName));
    Map<String, List<Map<String, String>>> sheetData = new HashMap<>();
    int currentRowNum = 0;
    try (Stream<Row> rowStream = sheet.openStream()) {
      Iterator<Row> iterator = rowStream.iterator();
      if (!iterator.hasNext()) {
        throw new ValidationException(ErrorCategory.VALIDATION.getCode(),
          fileName + " wajib diisi. Silakan upload file yang valid. Process - " + zipFileName
            + Constant.DOT);
      }
      // --- Read header row ---
      Row headerRow = null;
      while (FastExcelUtils.shouldProcessRow(config.getHeaderRowIndex(), iterator, currentRowNum)) {
        headerRow = iterator.next();
        currentRowNum++;
      }
      validateHeadersAndMaxCols(config, fileName, headerRow, zipFileName);
      // Step 1: Build a lookup map from header row
      Map<String, Integer> headerIndexMap = getHeaderIndexMap(headerRow);
      // Step 2: Map required columns
      Map<String, Integer> requiredColumnIndexMap =
        CommonUtils.getRequiredColumnIndexMap(zipFileName, fileName,
          sourceColumnToDestinationColumnMap,
          sourceColumnToMandatoryMap, config, headerIndexMap);
      // --- Validate join key column exists ---
      Integer joinKeyIndex = requiredColumnIndexMap.get(joinKey);
      // --- Skip to data start row ---
      while (FastExcelUtils.shouldProcessRow(config.getDataStartRowIndex(), iterator,
        currentRowNum)) {
        iterator.next();
        currentRowNum++;
      }
      // --- Process data rows ---
      int validDataRows = 0;
      processEachRowPerSheet(zipFileName, fileName, sourceColumnToMandatoryMap,
        productValidationErrorMap, config, iterator, currentRowNum, joinKeyIndex, validDataRows,
        requiredColumnIndexMap, sheetData);
    }
    // --- Assign to correct sheet map ---
    CommonUtils.distributeSheetDataToMap(productBasicInfoMap, productMediaInfoMap,
      productShippingInfoMap, productSalesInfoMap, fileName, sheetType, sheetData);
    log.info("Sheet {} processed successfully. Valid rows = {} ", fileName, sheetData.size());
  }

  public static void processEachZipFileForExternalUpload(String zipFileName, ZipFile zipFile,
    Map<String, String> excelFileNameToTypeMap, Map<String, SheetConfig> sheetConfigMap,
    PlatformConfig shopeeConfig, Map<String, String> sourceColumnToDestinationColumnMap,
    Map<String, Boolean> sourceColumnToMandatoryMap,
    Map<String, StringBuilder> productValidationErrorMap,
    Map<String, List<Map<String, String>>> productBasicInfoMap,
    Map<String, List<Map<String, String>>> productMediaInfoMap,
    Map<String, List<Map<String, String>>> productShippingInfoMap,
    Map<String, List<Map<String, String>>> productSalesInfoMap) throws ApplicationException {
    Enumeration<? extends ZipEntry> entries = zipFile.entries();
    while (entries.hasMoreElements()) {
      ZipEntry entry = entries.nextElement();
      String fileName = entry.getName();
      log.info("Processing file from ZIP: {} ", fileName);
      String sheetType = excelFileNameToTypeMap.get(fileName);
      SheetConfig config = sheetConfigMap.get(sheetType);
      String joinKey = shopeeConfig.getJoinKey();
      try (InputStream entryStream = zipFile.getInputStream(entry);
        ReadableWorkbook workbook = new ReadableWorkbook(entryStream)) {
        processEachFileForExternalUpload(zipFileName, sourceColumnToDestinationColumnMap,
          sourceColumnToMandatoryMap, productValidationErrorMap, productBasicInfoMap,
          productMediaInfoMap, productShippingInfoMap, productSalesInfoMap, workbook, config,
          fileName, joinKey, sheetType);
      } catch (IOException e) {
        throw new ApplicationException(ErrorCategory.VALIDATION,
          "Failed processing file " + fileName, e);
      }
    }
  }

  public static boolean shouldProcessRow(int targetRowIndex, Iterator<?> iterator,
    int currentRowNum) {
    return iterator.hasNext() && currentRowNum <= targetRowIndex;
  }

  public static Map<Integer, String> readHeaders(ReadableWorkbook workbook, int headerRowIndex,
    int offsetCellsFromLast, String bulkProcessCode) throws Exception {
    Sheet sheet = workbook.getFirstSheet();
    if (Objects.isNull(sheet)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        "No sheet found for bulkProcessCode : " + bulkProcessCode);
    }

    try (Stream<Row> rowStream = sheet.openStream()) {
      Iterator<Row> rowIterator = rowStream.iterator();
      int currentRowNum = 0;
      skipRowsUntilStartIndex(headerRowIndex, currentRowNum, rowIterator);
      if (!rowIterator.hasNext()) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Header row not found.");
      }
      Row headerRow = rowIterator.next();
      int cellCount = headerRow.getCellCount();
      int usableHeaderCellCount = Math.max(0, cellCount - offsetCellsFromLast);

      Map<Integer, String> headers = new TreeMap<>();
      for (int index = 0; index < usableHeaderCellCount; index++) {
        String header = getStringHeaderValue(headerRow.getCell(index), true).trim();
        headers.put(index, header);
      }
      return headers;
    }
  }

  public static List<Map<String, String>> readDataRows(ReadableWorkbook workbook,
    Map<Integer, String> headers, int dataStartRowIndex, int bulkMaxNumberOfRows,
    boolean validateBulkMaxNumberOfRows, String bulkProcessCode) throws Exception {
    return readDataRows(workbook, headers, dataStartRowIndex, bulkMaxNumberOfRows,
        validateBulkMaxNumberOfRows, bulkProcessCode, null);
  }

  public static List<Map<String, String>> readDataRows(ReadableWorkbook workbook,
    Map<Integer, String> headers, int dataStartRowIndex, int bulkMaxNumberOfRows,
    boolean validateBulkMaxNumberOfRows, String bulkProcessCode, Set<String> columnsToParseAsDouble) throws Exception {
    Sheet sheet = workbook.getFirstSheet();
    List<Map<String, String>> productDataFromExcel = new ArrayList<>();

    try (Stream<Row> rowStream = sheet.openStream()) {
      Iterator<Row> rowIterator = rowStream.iterator();
      int currentRowNum = 0;

      currentRowNum = skipRowsUntilStartIndex(dataStartRowIndex, currentRowNum, rowIterator);

      int usableHeaderCellCount = headers.size();
      int logicalDataRowIndex = 0;

      while (rowIterator.hasNext()) {
        Row row = rowIterator.next();
        currentRowNum++;
        logicalDataRowIndex++;

        if (validateBulkMaxNumberOfRows && logicalDataRowIndex > bulkMaxNumberOfRows) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            "Bulk file exceeds maximum allowed rows: " + bulkMaxNumberOfRows + " for "
              + "bulkProcessCode : " + bulkProcessCode);
        }

        Map<String, String> data = new TreeMap<>();
        boolean allBlank = true;
        int actualCellCount = row.getCellCount();

        for (int index = 0; index < usableHeaderCellCount; index++) {
          String header = headers.get(index);
          if (StringUtils.isBlank(header)) continue;
          Cell cell = index < actualCellCount ? row.getCell(index) : null;
          boolean isEanOrUpc = header.equals(BulkParameters.EAN_OR_UPC);
          String value = CollectionUtils.isNotEmpty(columnsToParseAsDouble) ?
              getStringHeaderValueForBulkUpdate(cell, isEanOrUpc, header, columnsToParseAsDouble).trim() :
              getStringHeaderValue(cell, isEanOrUpc).trim();
          if (!value.isEmpty()) allBlank = false;
          data.put(header, value);
        }

        if (!allBlank) {
          data.put(Constant.DATA_ROW_NUMBER, String.valueOf(row.getRowNum()));
          productDataFromExcel.add(data);
        }
      }
    }
    return productDataFromExcel;
  }

  public static int skipRowsUntilStartIndex(int dataStartRowIndex, int currentRowNum,
    Iterator<Row> rowIterator) {
    while (currentRowNum < dataStartRowIndex && rowIterator.hasNext()) {
      rowIterator.next();
      currentRowNum++;
    }
    return currentRowNum;
  }

  public static String getStringHeaderValue(Cell cell, boolean stringValue) {
    if (Objects.isNull(cell)) {
      return StringUtils.EMPTY;
    }
    String rawValue = cell.getRawValue();
    if (StringUtils.isBlank(rawValue)) {
      return StringUtils.EMPTY;
    }
    if (!stringValue) {
      try {
        return String.valueOf(Double.parseDouble(rawValue));
      } catch (NumberFormatException e) {
        // Not a numeric value, return as-is
        return rawValue;
      }
    }
    return rawValue;
  }

  public static String getStringHeaderValueForBulkUpdate(Cell cell, boolean stringValue,
    String columnName, Set<String> columnsToParseAsDouble) {
    if (Objects.isNull(cell)) {
      return StringUtils.EMPTY;
    }
    String cellValue = cell.getText();
    if (StringUtils.isBlank(cellValue)) {
      return StringUtils.EMPTY;
    }
    if (!stringValue && shouldParseAsDouble(columnName, columnsToParseAsDouble)) {
      try {
        return String.valueOf(Double.parseDouble(cellValue));
      } catch (NumberFormatException e) {
        return cellValue;
      }
    }
    return cellValue;
  }

  private static boolean shouldParseAsDouble(String columnName,
    Set<String> columnsToParseAsDouble) {
    if (StringUtils.isBlank(columnName) || CollectionUtils.isEmpty(columnsToParseAsDouble)) {
      return false;
    }
    String trimmedColumnName = columnName.trim();
    return columnsToParseAsDouble.stream().anyMatch(
      configuredColumn -> StringUtils.isNotBlank(configuredColumn)
        && trimmedColumnName.equalsIgnoreCase(configuredColumn.trim()));
  }
}
