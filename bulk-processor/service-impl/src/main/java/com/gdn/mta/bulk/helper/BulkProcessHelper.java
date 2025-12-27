package com.gdn.mta.bulk.helper;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFPalette;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.hssf.util.HSSFColor;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.DataFormat;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellUtil;
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.models.ExcelHeaderNames;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkOrderResponse;
import com.gdn.mta.bulk.models.download.responsedata.OrderItemSummaryResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.CSVUtil;
import com.gdn.partners.bulk.util.Constant;

/**
 * Created by keshashah on 25/10/16.
 */
public abstract class BulkProcessHelper {

  private static final Logger LOGGER = LoggerFactory.getLogger(BulkProcessHelper.class);
  private static final ObjectMapper objectMapper = new ObjectMapper();

  /**
   * Get CSV Headers for each bulk entity with respected field in response model
   *
   * @param request
   * @return
   */
  public abstract BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request);

  public byte[] generateCsvFile(BulkCsvModel csvModel, BulkDataResponse dataResponse)
      throws IllegalAccessException, NoSuchMethodException, InvocationTargetException, IOException {
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    PrintWriter printWriter = new PrintWriter(outputStream);
    CSVUtil csvUtil = new CSVUtil(printWriter);
    generateOrderCsvHeader(csvUtil, csvModel);
    SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm");
    if (dataResponse instanceof BulkOrderResponse) {
      BulkOrderResponse bulkOrderResponse = (BulkOrderResponse) dataResponse;

      for (OrderItemSummaryResponse item : bulkOrderResponse.getResponseList()) {
        int index = 0;
        for (String header : csvModel.getHeaderList()) {
          index = index + 1;
          Object property =
              PropertyUtils.getProperty(item, csvModel.getHeaderToFieldMap().get(header));
          String propertyValue = Constant.DASH;
          if (property != null) {
            if (property instanceof Date) {
              propertyValue = sdf.format(property);
            } else {
              propertyValue = property.toString();
            }
          }
          if (index != csvModel.getHeaderList().size()) {
            csvUtil.print(propertyValue);
            continue;
          }
          csvUtil.println(propertyValue);
        }

      }
    }
    printWriter.close();
    outputStream.flush();
    outputStream.close();
    return outputStream.toByteArray();
  }

  public abstract List<String> getHeaderList(BulkDataResponse bulkDataResponse) throws Exception;

  public Workbook generateDataSheet(List<String> headers, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth) throws IOException {
    Workbook workbook = new SXSSFWorkbook();
    List<List<String>> headerList = new ArrayList<>();
    headerList.add(headers);
    return addSheetsAndGetWorkbookForCreation(headerList, rowDataList, workbook, BulkParameters.DATA_SHEET, true);
  }

  public Workbook generateDataSheetWithHSSFWorkBook(List<List<String>> headerList, List<List<String>> rowDataList,
      String sheetName, boolean mergeEmptyValueCells) {
    Workbook workbook = new HSSFWorkbook();
    return addSheetsAndGetWorkbookForCreation(headerList, rowDataList, workbook, sheetName, mergeEmptyValueCells);
  }

  private Workbook addSheetsAndGetWorkbookForCreation(List<List<String>> headerList, List<List<String>> rowDataList,
      Workbook workbook, String sheetName, boolean mergeEmptyValueCells) {
    Sheet dataSheet = workbook.createSheet(sheetName);
    if (SXSSFSheet.class.isInstance(dataSheet)) {
      ((SXSSFSheet) dataSheet).trackAllColumnsForAutoSizing();
    }
    DataFormat format = workbook.createDataFormat();
    CellStyle styleForSellerSKU = workbook.createCellStyle();
    CellStyle externalProductListHeaderStyle = workbook.createCellStyle();
    setLightGreenCellBackground(externalProductListHeaderStyle);
    setThinBlackBorderCellStyle(externalProductListHeaderStyle);
    setColumnWidthForBfbStatus(headerList, dataSheet);
    Row row;
    int rowId = 0;
    int sellerSkuColumnId = -1;
    for (List<String> headers : headerList) {
      row = dataSheet.createRow(rowId);
      row.setHeightInPoints(BulkParameters.HEADER_ROW_HEIGHT);
      int cellIndex = 0;
      int mergeStartIndex = 0;
      int mergeEndIndex = 0;
      sellerSkuColumnId = headers.indexOf(ExcelHeaderNames.SELLER_SKU);
      for (String headerValue : headers) {
        Cell cell = row.createCell((short) cellIndex);
        cell.setCellType(Cell.CELL_TYPE_STRING);
        cell.setCellValue(headerValue);
        setCellStyleForExternalProductList(cell, headerValue, externalProductListHeaderStyle);
        if (StringUtils.isEmpty(headerValue) && mergeEmptyValueCells) {
          mergeEndIndex++;
        } else {
          if (mergeStartIndex != mergeEndIndex) {
            CellRangeAddress mergeCellRangeAddress = new CellRangeAddress(rowId, rowId, mergeStartIndex, mergeEndIndex);
            dataSheet.addMergedRegion(mergeCellRangeAddress);
          }
          mergeStartIndex = cellIndex;
          mergeEndIndex = cellIndex;
        }
        cellIndex++;
      }
      rowId++;
    }
    for (List<String> rowData : rowDataList) {
      row = dataSheet.createRow(rowId);
      rowId++;
      int cellId = 0;
      for (String cellValue : rowData) {
        Cell cell = row.createCell(cellId);
        if (sellerSkuColumnId == cellId) {
          styleForSellerSKU.setDataFormat(format.getFormat("@"));
          cell.setCellStyle(styleForSellerSKU);
        }
        cell.setCellValue(Optional.ofNullable(cellValue).orElse(StringUtils.EMPTY));
        cellId++;
      }
    }
    if (SXSSFSheet.class.isInstance(dataSheet) && CollectionUtils.isNotEmpty(headerList.get(0))
        && CollectionUtils.isNotEmpty(rowDataList)) {
      for (int i = 0; i < dataSheet.getRow(dataSheet.getLastRowNum()).getLastCellNum() - 1; i++) {
        dataSheet.autoSizeColumn(i);
      }
    }
    return workbook;
  }

  protected static void setColumnWidthForBfbStatus(List<List<String>> headerList, Sheet dataSheet) {
    if (headerList.get(0).contains(BulkParameters.AMPHI_BFB_STATUS)) {
      dataSheet.setColumnWidth(headerList.get(0).indexOf(BulkParameters.AMPHI_BFB_STATUS),
          Integer.parseInt(BulkParameters.AMPHI_BFB_STATUS_COLUMN_WIDTH));
    }
  }

  public static short getClosestHSSFColorIndex(byte[] rgb, Workbook workbook) {
    HSSFPalette palette = ((HSSFWorkbook) workbook).getCustomPalette();
    HSSFColor closestColor = palette.findColor(rgb[0], rgb[1], rgb[2]);
    return closestColor != null ?
        closestColor.getIndex() :
        HSSFColor.HSSFColorPredefined.LIGHT_ORANGE.getIndex();
  }

  private static void setThinBlackBorderCellStyle(CellStyle cellStyle) {
    cellStyle.setBorderBottom(BorderStyle.THIN);
    cellStyle.setBorderLeft(BorderStyle.THIN);
    cellStyle.setBorderTop(BorderStyle.THIN);
    cellStyle.setBorderRight(BorderStyle.THIN);
    cellStyle.setBottomBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setLeftBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setTopBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setRightBorderColor(IndexedColors.BLACK.getIndex());
  }

  private static void setLightGreenCellBackground(CellStyle cellStyle) {
    cellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    cellStyle.setFillForegroundColor(IndexedColors.LIGHT_GREEN.getIndex());
  }

  private void setCellStyleForExternalProductList(Cell cell, String headerValue,
      CellStyle cellStyle) {
    if (BulkParameters.BLIBLI_PRODUCT_SKU.equals(headerValue)
        || BulkParameters.PARENT_PRODUCT_NAME.equals(headerValue)
        || BulkParameters.AMPHI_SKU_STATUS.equals(headerValue)
        || BulkParameters.EXTERNAL_SKU_STATUS.equals(headerValue)) {
      cell.setCellStyle(cellStyle);
    }
    if (BulkParameters.AMPHI_BFB_STATUS.equals(headerValue)) {
      CellUtil.setCellStyleProperty(cell, CellUtil.WRAP_TEXT, true);
    }
  }

  private void generateOrderCsvHeader(CSVUtil csvUtil, BulkCsvModel csvModel) {
    int index = 0;
    for (String header : csvModel.getHeaderList()) {
      if (index == csvModel.getHeaderList().size() - 1) {
        csvUtil.println(header);
      } else {
        csvUtil.print(header);
      }
      index++;
    }
  }

  public void generateFile(String filepath, byte[] content) throws IOException {
    File file = new File(filepath);
    if (file.exists()) {
      file.delete();
    }
    file.createNewFile();
    FileOutputStream fileOutputStream = new FileOutputStream(file);

    try {
      fileOutputStream.write(content);
    } catch (IOException e) {
      LOGGER.error("Error creating file {}", filepath);
    } finally {
      fileOutputStream.close();

    }
  }

  public String getUserName(String email) {
    return email.split("@")[0];
  }

  public abstract Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response)
      throws Exception;

  public abstract List<List<String>> getRowData(BulkDataResponse response);

  public abstract String getDirectory(BulkDownloadRequest request);

  public String getFilePath(String directoryPath, String filename) {
    return new StringBuilder().append(directoryPath).append(File.separator).append(filename)
        .toString();
  }

  /**
   * to get email param for order/product download summary.
   *
   * @param request
   * @param lang
   * @return
   */
  public abstract Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang);

  public abstract int getRecordsUpdated(BulkDataResponse response);
}
