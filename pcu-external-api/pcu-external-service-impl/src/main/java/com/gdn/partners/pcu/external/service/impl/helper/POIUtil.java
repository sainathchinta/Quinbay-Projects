package com.gdn.partners.pcu.external.service.impl.helper;

import static com.gdn.partners.pcu.external.model.Constants.DATE_FORMAT;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.IntStream;

import com.gdn.partners.pcu.external.model.Constants;
import org.apache.commons.lang.StringUtils;
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
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFColor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.gdn.partners.pcu.external.model.ExcelHeaderNames;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandData;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandDataRequest;
import com.gdn.partners.pcu.external.web.model.response.BulkSelectedDownloadHeaders;
import com.gdn.x.businesspartner.dto.PickupPointDTO;

public abstract class POIUtil<T> {
  static Logger logger = LoggerFactory.getLogger(POIUtil.class);
  private static final byte[] LIGHT_GREEN_RGB = {(byte) 226, (byte) 239, (byte) 218};
  private static final byte[] GREEN_RGB = {(byte) 169, (byte) 208, (byte) 142};
  private static final byte[] PEACH_RGB = {(byte) 248, (byte) 204, (byte) 173};
  private static final byte[] LIGHT_PEACH_RGB = {(byte) 252, (byte) 228, (byte) 214};
  private static final int FOURTH_ROW_INDEX = 3;
  private static final int FIRST_ROW_INDEX = 0;
  private static final int FOURTH_ROW_HEIGHT = 96;
  private static final int FIRST_ROW_HEIGHT = 40;

  /**
   * Creates a new instance of POIExcelReader
   */
  public POIUtil() {}

  public static SXSSFWorkbook generateXLFile(Map<Integer, List<String>> products, List<PickupPointDTO> pickupPoints,
      boolean isWhitelistedSeller, BulkSelectedDownloadHeaders bulkSelectedDownloadHeaders,
      int bulkUpdateTemplateColumnWidth) {
    List<String> primaryHeaderList = bulkSelectedDownloadHeaders.getPrimaryHeaders();
    List<String> secondaryHeaderList = bulkSelectedDownloadHeaders.getSecondaryHeaders();
    List<String> tertiaryHeaderList = bulkSelectedDownloadHeaders.getTertiaryHeaders();
    List<String> quaternaryHeaderList = bulkSelectedDownloadHeaders.getQuaternaryHeaders();
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet("Data");
    ((SXSSFSheet) dataSheet).trackAllColumnsForAutoSizing();
    Sheet pickupPointSheet = workbook.createSheet("Toko");
    DataFormat format = workbook.createDataFormat();
    CellStyle styleForSellerSKU = workbook.createCellStyle();
    int rowindex = 0;
    Row row;
    for (PickupPointDTO pickupPoint : pickupPoints) {
      row = pickupPointSheet.createRow((short) rowindex++);
      Cell cell = row.createCell((short) 0);
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(pickupPoint.getCode());

      cell = row.createCell((short) 1);
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(pickupPoint.getName());

      if(isWhitelistedSeller && pickupPoint.isFbbActivated()){
        cell = row.createCell((short) 2);
        cell.setCellType(Cell.CELL_TYPE_STRING);
        cell.setCellValue(Constants.FBB);
      }
    }
    int rowid = 0;
    createHeaderRow(dataSheet, workbook, rowid++, primaryHeaderList, tertiaryHeaderList, false, false, true);
    createHeaderRow(dataSheet, workbook, rowid++, secondaryHeaderList, tertiaryHeaderList, false, false, false);
    createHeaderRow(dataSheet, workbook, rowid++, tertiaryHeaderList, tertiaryHeaderList, false, false, false);
    createHeaderRow(dataSheet, workbook, rowid++, quaternaryHeaderList, tertiaryHeaderList, true, true, false);
    int columnWidth = bulkUpdateTemplateColumnWidth * 256;
    IntStream.range(0, primaryHeaderList.size())
        .forEach(col -> dataSheet.setColumnWidth(col, columnWidth));
    dataSheet.getRow(FOURTH_ROW_INDEX).setHeightInPoints(FOURTH_ROW_HEIGHT);
    dataSheet.getRow(FIRST_ROW_INDEX).setHeightInPoints(FIRST_ROW_HEIGHT);
    Set<Integer> keyid = products.keySet();
    int sellerSkuColumnIndex = primaryHeaderList.indexOf(ExcelHeaderNames.SELLER_SKU);
    for (Integer key : keyid) {
      row = dataSheet.createRow(rowid++);
      List<String> attributes = products.get(key);
      int cellId = 0;
      for (String attribute : attributes) {
        Cell cell = row.createCell(cellId);
        if (sellerSkuColumnIndex == cellId) {
          styleForSellerSKU.setDataFormat(format.getFormat("@"));
          cell.setCellStyle(styleForSellerSKU);
        }
        cellId++;
        cell.setCellValue(Optional.ofNullable(attribute).orElse(StringUtils.EMPTY));
      }
    }
    return workbook;
  }

  private static void createHeaderRow(Sheet dataSheet, SXSSFWorkbook workbook, int rowIndex, List<String> headers,
      List<String> tertiaryHeaders, boolean quaternaryHeader, boolean applyTopBottomBorder, boolean primaryHeader) {
    Row row = dataSheet.createRow(rowIndex);
    int cellIndex = 0;
    int tertiaryHeaderIterator = 0;

    for (String header : headers) {
      Cell cell = row.createCell(cellIndex++);
      cell.setCellType(CellType.STRING);
      cell.setCellValue(header);
      CellStyle cellStyle = createCellStyle(workbook, header, tertiaryHeaders, tertiaryHeaderIterator, quaternaryHeader,
          applyTopBottomBorder, primaryHeader);
      cell.setCellStyle(cellStyle);
      tertiaryHeaderIterator++;
    }
  }

  private static CellStyle createCellStyle(SXSSFWorkbook workbook, String header, List<String> tertiaryHeaders,
      int tertiaryHeaderIterator, boolean quaternaryHeader, boolean applyTopBottomBorder, boolean primaryHeader) {
    CellStyle cellStyle = workbook.createCellStyle();
    cellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    byte[] rgb = getColorForHeader(tertiaryHeaders, tertiaryHeaderIterator, quaternaryHeader);
    XSSFColor customColor = new XSSFColor(rgb, null);
    ((XSSFCellStyle) cellStyle).setFillForegroundColor(customColor);
    setBorders(cellStyle, applyTopBottomBorder);
    setFontStyle(workbook, cellStyle, header, primaryHeader, quaternaryHeader);
    setAlignment(cellStyle, primaryHeader);
    return cellStyle;
  }

  private static byte[] getColorForHeader(List<String> tertiaryHeaders, int tertiaryHeaderIterator,
      boolean quaternaryHeader) {
    if (Constants.EDITABLE_COLUMN_ID.equals(tertiaryHeaders.get(tertiaryHeaderIterator))) {
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

  private static void setFontStyle(SXSSFWorkbook workbook, CellStyle cellStyle, String header, boolean primaryHeader,
      boolean quaternaryHeader) {
    Font boldFont = workbook.createFont();
    boldFont.setBold(!quaternaryHeader);
    boldFont.setFontHeightInPoints(primaryHeader ? (short) 14 : (short) 12);
    if (Constants.MANDATORY_COLUMN_ID.equals(header)) {
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

  public static SXSSFWorkbook generateWorkbookForMPPTemplate(List<String> headerList, List<PickupPointDTO> pickupPoints,
    String pickupPointSheetName, String pickupPointCodeHeader, String pickupPointNameHeader,
    boolean isWhiteListedMerchant) {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet("Data");
    ((SXSSFSheet) dataSheet).trackAllColumnsForAutoSizing();
    Sheet pickupPointSheet = workbook.createSheet(pickupPointSheetName);
    ((SXSSFSheet) pickupPointSheet).trackAllColumnsForAutoSizing();
    workbook.createSheet(Constants.BULK_UPSERT_SHEET);
    workbook.setSheetHidden(workbook.getSheetIndex(Constants.BULK_UPSERT_SHEET), true);
    int pickupPointSheetRowIndex = 0;
    int dataSheetRowIndex = 0;
    Row row = pickupPointSheet.createRow((short) pickupPointSheetRowIndex++);
    CellStyle cellStyle = workbook.createCellStyle();
    setCellBackground(cellStyle, FillPatternType.SOLID_FOREGROUND, IndexedColors.LIGHT_YELLOW.getIndex());
    Cell cell = row.createCell((short) 0);
    setCellProperties(cell, CellType.STRING, pickupPointCodeHeader, cellStyle);
    cell = row.createCell((short) 1);
    setCellProperties(cell, CellType.STRING, pickupPointNameHeader, cellStyle);
    CellStyle pickupPointsCellStyle = workbook.createCellStyle();
    for (PickupPointDTO pickupPointDTO : pickupPoints) {
      row = pickupPointSheet.createRow((short) pickupPointSheetRowIndex++);
      cell = row.createCell((short) 0);
      setCellProperties(cell, CellType.STRING, pickupPointDTO.getCode(), pickupPointsCellStyle);
      cell = row.createCell((short) 1);
      setCellProperties(cell, CellType.STRING, pickupPointDTO.getName(), pickupPointsCellStyle);
      if (isWhiteListedMerchant && pickupPointDTO.isFbbActivated()) {
        cell = row.createCell((short) 2);
        setCellProperties(cell, CellType.STRING, Constants.FBB, pickupPointsCellStyle);
        pickupPointSheet.autoSizeColumn(2);
      }
    }
    row = dataSheet.createRow((short) dataSheetRowIndex++);
    int cellIndex = 0;
    for (String header : headerList) {
      Cell dataSheetCell = row.createCell((short) cellIndex++);
      setCellProperties(dataSheetCell, CellType.STRING, header, cellStyle);
    }
    for (int i = 0; i < dataSheet.getRow(dataSheet.getLastRowNum()).getLastCellNum(); i++) {
      dataSheet.autoSizeColumn(i);
    }
    for (int i = 0; i < pickupPointSheet.getRow(pickupPointSheet.getLastRowNum()).getLastCellNum(); i++) {
      pickupPointSheet.autoSizeColumn(i);
    }
    return workbook;
  }

  public static SXSSFWorkbook generateWorkbookForBrandAuth(Set<String> headerSet,
      BulkBrandDataRequest bulkBrandDataRequest) {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet brandSheet = workbook.createSheet("BrandAuthData");
    ((SXSSFSheet) brandSheet).trackAllColumnsForAutoSizing();
    Row row;
    int cellIndex1 = 0;
    int rowindex = 1;
    List<BulkBrandData> bulkBrandData = bulkBrandDataRequest.getDownloadRequest();
    row = brandSheet.createRow((short) 0);
    for (String header : headerSet) {
      Cell cell = row.createCell((short) cellIndex1++);
      cell.setCellType(CellType.STRING);
      cell.setCellValue(header);
    }
    for (int i = 0; i < headerSet.size(); i++) {
      brandSheet.autoSizeColumn(i);
    }
    for (BulkBrandData brandData : bulkBrandData) {
      int cellIndex = 0;
      row = brandSheet.createRow((short) rowindex++);


      Cell cell1 = row.createCell((short) cellIndex++);
      cell1.setCellType(CellType.STRING);
      cell1.setCellValue(brandData.getBrandCode());

      Cell cell2 = row.createCell((short) cellIndex++);
      cell2.setCellType(CellType.STRING);
      cell2.setCellValue(brandData.getBrandName());

      Cell cell3 = row.createCell((short) cellIndex++);
      cell3.setCellType(CellType.STRING);
      cell3.setCellValue(brandData.getSellerCode());

      Cell cell4 = row.createCell((short) cellIndex++);
      cell4.setCellType(CellType.STRING);
      cell4.setCellValue(brandData.getSellerName());

      Cell cell5 = row.createCell((short) cellIndex++);
      cell5.setCellType(CellType.STRING);
      Date date = new Date(brandData.getAuthStartDate());
      SimpleDateFormat formatter = new SimpleDateFormat(DATE_FORMAT);
      String formattedDate = formatter.format(date);
      cell5.setCellValue(formattedDate);

      Cell cell6 = row.createCell((short) cellIndex);
      cell6.setCellType(CellType.STRING);
      date = new Date(brandData.getAuthEndDate());
      formattedDate = formatter.format(date);
      cell6.setCellValue(formattedDate);
    }
    return workbook;
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

  private static void setThinBlackBorderCellStyleWithoutTopBottom(CellStyle cellStyle) {
    cellStyle.setBorderLeft(BorderStyle.THIN);
    cellStyle.setBorderRight(BorderStyle.THIN);
    cellStyle.setLeftBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setRightBorderColor(IndexedColors.BLACK.getIndex());
  }

  private static void setLightGreenCellBackground(CellStyle cellStyle) {
    cellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    byte[] rgb = new byte[]{(byte) 248, (byte) 204, (byte) 173}; // Adjust RGB values
    XSSFColor customColor = new XSSFColor(rgb, null);
    ((XSSFCellStyle) cellStyle).setFillForegroundColor(customColor);
  }

  private static void setCellBackground(CellStyle cellStyle, FillPatternType fillPatternType, short foregroundColor) {
    cellStyle.setFillPattern(fillPatternType);
    cellStyle.setFillForegroundColor(foregroundColor);
  }

  private static void setCellProperties(Cell cell, CellType cellType, String cellValue, CellStyle cellStyle) {
    cell.setCellType(cellType);
    cell.setCellValue(cellValue);
    cell.setCellStyle(cellStyle);
  }
}
