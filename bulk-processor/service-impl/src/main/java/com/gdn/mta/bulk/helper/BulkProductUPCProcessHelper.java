package com.gdn.mta.bulk.helper;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DataFormat;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFColor;
import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductLiteResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;

import lombok.extern.slf4j.Slf4j;


@Slf4j
@Component(value = "bulkProductUPCProcessHelper")
public class BulkProductUPCProcessHelper extends BulkProcessHelper {
  public static final String REQ_ID = "reqId";
  private static final List<String> PRIMARY_HEADER_LIST =
      List.of(BulkParameters.BLIBLI_PRODUCT_SKU, BulkParameters.PARENT_PRODUCT_NAME, BulkParameters.BLIBLI_SKU,
          BulkParameters.PRODUCT_NAME, BulkParameters.EAN_OR_UPC);

  private static final List<String> SECONDARY_HEADERS =
      List.of(BulkParameters.MANDATORY, BulkParameters.OPTIONAL, BulkParameters.MANDATORY, BulkParameters.OPTIONAL,
          BulkParameters.OPTIONAL);

  private static final List<String> TERTIARY_HEADERS =
      List.of(BulkParameters.NOT_EDITABLE, BulkParameters.NOT_EDITABLE, BulkParameters.NOT_EDITABLE,
          BulkParameters.NOT_EDITABLE, BulkParameters.EDITABLE);

  private static final List<String> QUATERNARY_HEADERS =
      List.of(BulkParameters.BLIBLI_PRODUCT_SKU_DESC, BulkParameters.PARENT_PRODUCT_NAME_DESC,
          BulkParameters.BLIBLI_SKU_DESC, BulkParameters.PRODUCT_NAME_DESC, BulkParameters.UPC_CODE_NAME_DESC);

  public static final List<List<String>> HEADER_LIST =
      List.of(PRIMARY_HEADER_LIST, SECONDARY_HEADERS, TERTIARY_HEADERS, QUATERNARY_HEADERS);


  private static final byte[] LIGHT_GREEN_RGB = {(byte) 226, (byte) 239, (byte) 218};
  private static final byte[] GREEN_RGB = {(byte) 169, (byte) 208, (byte) 142};
  private static final byte[] PEACH_RGB = {(byte) 248, (byte) 204, (byte) 173};
  private static final byte[] LIGHT_PEACH_RGB = {(byte) 252, (byte) 228, (byte) 214};
  private static final int FIRST_ROW_INDEX = 0;
  private static final int FIRST_ROW_HEIGHT = 40;
  public static final String NAME = "name";
  public static final String CELL_TEXT_FORMAT ="@";
  public static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) throws Exception {
    return HEADER_LIST.stream().map(inner -> String.join(",", inner)).collect(Collectors.toList());
  }


  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }


  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkProductLiteResponse productResponse = (BulkProductLiteResponse) response;
    return productResponse.getProductContentList();
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.PRODUCT_EAN) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(NAME, getUserName(request.getUsername()));
    emailParameters.put(REQ_ID, request.getRequestId());
    emailParameters.put(BUSINESS_PARTNER_CODE, request.getMerchantId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM,
        MessageUtil.getMessage(EmailConstants.PRODUCT_DOWNLOAD_TEMPLATE_ID, lang));
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM,
        MessageUtil.getMessage(EmailConstants.PRODUCT_MAIL_SUBJECT, lang));
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BulkProductLiteResponse productResponse = (BulkProductLiteResponse) response;
    return productResponse.getProductContentList().size();
  }


  public Workbook generateDataSheet(List<String> headers, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth) throws IOException {
    Workbook workbook = new SXSSFWorkbook();
    return addSheetsAndGetWorkbook( rowDataList, workbook, bulkUpdateTemplateColumnWidth);
  }

  private Workbook addSheetsAndGetWorkbook(List<List<String>> rowDataList,
      Workbook workbook, int bulkUpdateTemplateColumnWidth) {
    Sheet dataSheet = workbook.createSheet(BulkParameters.DATA_SHEET);
    List<String> primaryHeaders = HEADER_LIST.get(0);
    List<String> secondaryHeaders = HEADER_LIST.get(1);
    List<String> tertiaryHeaders = HEADER_LIST.get(2);
    List<String> quaternaryHeaders = HEADER_LIST.get(3);
    Row row;
    int rowId = 0;

    createHeaderRow(dataSheet, workbook, rowId++, primaryHeaders, tertiaryHeaders, false, false, true);
    createHeaderRow(dataSheet, workbook, rowId++, secondaryHeaders, tertiaryHeaders, false, false, false);
    createHeaderRow(dataSheet, workbook, rowId++, tertiaryHeaders, tertiaryHeaders, false, false, false);
    createHeaderRow(dataSheet, workbook, rowId++, quaternaryHeaders, tertiaryHeaders, true, true, false);
    int columnWidth = bulkUpdateTemplateColumnWidth * 256;
    IntStream.range(0, primaryHeaders.size()).forEach(col -> dataSheet.setColumnWidth(col, columnWidth));
    dataSheet.getRow(FIRST_ROW_INDEX).setHeightInPoints(FIRST_ROW_HEIGHT);
    for (List<String> rowData : rowDataList) {
      row = dataSheet.createRow(rowId);
      rowId++;
      int cellId = 0;
      for (String cellValue : rowData) {
        Cell cell = row.createCell(cellId);
        cell.setCellValue(Optional.ofNullable(cellValue).orElse(StringUtils.EMPTY));
        CellStyle textStyle = workbook.createCellStyle();
        DataFormat format = workbook.createDataFormat();
        textStyle.setDataFormat(format.getFormat(CELL_TEXT_FORMAT));
        cell.setCellStyle(textStyle);
        cellId++;
      }
    }
    return workbook;
  }

  private static void createHeaderRow(Sheet dataSheet, Workbook workbook, int rowIndex, List<String> headers,
      List<String> tertiaryHeaders, boolean quaternaryHeader, boolean applyTopBottomBorder, boolean primaryHeader) {
    Row row = dataSheet.createRow(rowIndex);
    int cellIndex = 0;
    int tertiaryHeaderIterator = 0;
    for (String header : headers) {
      Cell cell = row.createCell(cellIndex++);
      cell.setCellType(CellType.STRING);
      CellStyle cellStyle = createCellStyle(workbook, header, tertiaryHeaders, tertiaryHeaderIterator, quaternaryHeader,
          applyTopBottomBorder, primaryHeader);
      cellStyle.setWrapText(true);
      DataFormat format = workbook.createDataFormat();
      cellStyle.setDataFormat(format.getFormat(CELL_TEXT_FORMAT));
      cell.setCellStyle(cellStyle);
      cell.setCellValue(header);
      tertiaryHeaderIterator++;
    }
  }

  private static CellStyle createCellStyle(Workbook workbook, String header, List<String> tertiaryHeaders,
      int tertiaryHeaderIterator, boolean quaternaryHeader, boolean applyTopBottomBorder, boolean primaryHeader) {
    CellStyle cellStyle = workbook.createCellStyle();
    cellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    byte[] rgb = getColorForHeader(tertiaryHeaders, tertiaryHeaderIterator, quaternaryHeader);
    XSSFCellStyle xssfCellStyle = (XSSFCellStyle) cellStyle;
    XSSFColor customColor = new XSSFColor(rgb, null);
    xssfCellStyle.setFillForegroundColor(customColor);
    setBorders(cellStyle, applyTopBottomBorder);
    setFontStyle(workbook, cellStyle, header, primaryHeader, quaternaryHeader);
    setAlignment(cellStyle, primaryHeader);
    return cellStyle;
  }

  private static byte[] getColorForHeader(List<String> tertiaryHeaders, int tertiaryHeaderIterator,
      boolean quaternaryHeader) {
    if (BulkParameters.EDITABLE.equals(tertiaryHeaders.get(tertiaryHeaderIterator))) {
      return quaternaryHeader ? LIGHT_GREEN_RGB : GREEN_RGB;
    }
    return quaternaryHeader ? LIGHT_PEACH_RGB : PEACH_RGB;
  }

  private static void setBorders(CellStyle cellStyle, boolean applyTopBottomBorder) {
    cellStyle.setBorderLeft(BorderStyle.THIN);
    cellStyle.setBorderRight(BorderStyle.THIN);
    cellStyle.setLeftBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setRightBorderColor(IndexedColors.BLACK.getIndex());
    if (applyTopBottomBorder) {
      cellStyle.setBorderTop(BorderStyle.THIN);
      cellStyle.setTopBorderColor(IndexedColors.BLACK.getIndex());
      cellStyle.setBorderBottom(BorderStyle.THIN);
      cellStyle.setBottomBorderColor(IndexedColors.BLACK.getIndex());
    }
  }

  private static void setFontStyle(Workbook workbook, CellStyle cellStyle, String header, boolean primaryHeader,
      boolean quaternaryHeader) {
    Font boldFont = workbook.createFont();
    boldFont.setBold(!quaternaryHeader);
    boldFont.setFontHeightInPoints((short) (primaryHeader ? 14 : 12));
    if (BulkParameters.MANDATORY.equals(header)) {
      boldFont.setColor(IndexedColors.RED.getIndex());
    }
    cellStyle.setFont(boldFont);
  }

  private static void setAlignment(CellStyle cellStyle, boolean primaryHeader) {
    cellStyle.setWrapText(true);
    cellStyle.setAlignment(HorizontalAlignment.CENTER);
    cellStyle.setVerticalAlignment(VerticalAlignment.CENTER);
    if (primaryHeader) {
      cellStyle.setIndention((short) 4);
    }
  }
}
