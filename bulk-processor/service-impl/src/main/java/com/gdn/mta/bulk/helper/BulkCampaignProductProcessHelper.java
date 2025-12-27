package com.gdn.mta.bulk.helper;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.ComparisonOperator;
import org.apache.poi.ss.usermodel.ConditionalFormattingRule;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.PatternFormatting;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.SheetConditionalFormatting;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkCampaignProductResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.service.PromoAnalyticsOutboundService;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.promo.analytics.web.model.dto.ItemInfoDto;
import com.gdn.partners.promo.analytics.web.model.request.LowestPriceRecommendationRequest;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceRecommendationResponse;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceResponse;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

@Component("bulkCampaignProductProcessHelper")
public class BulkCampaignProductProcessHelper extends BulkProcessHelper {

  private static final String PRICE_CUT_FORMULA_MPP_CAMPAIGN_RECOMMENDATION =
      "IF(INT(G:G)-INT(M:M)=INT(G:G),\"0\",IF(INT(G:G)>=INT(M:M),INT(G:G)-INT(M:M),\"KESALAHAN !!! Harga Akhir Tidak Bisa Lebih Dari Harga Jual\"))";

  private static final String DISCOUNT_PERCENTAGE_FORMULA_MPP_CAMPAIGN_RECOMMENDATION =
      "IF(INT(M:M)>INT(G:G), \"KESALAHAN !!! Harga Akhir Tidak Bisa Lebih Dari Harga Jual\", IF(INT(H:H)/INT(G:G)=0,"
          + "\"-\",(INT(H:H)/INT(G:G))*100%))";
  private static final String DISCOUNT_PERCENTAGE_FORMULA_MPP =
      "IF(INT(L:L)>INT(G:G), \"KESALAHAN !!! Harga Akhir Tidak Bisa Lebih Dari Harga Jual\", IF(INT(H:H)/INT(G:G)=0,"
          + "\"-\",(INT(H:H)/INT(G:G))*100%))";
  private static final String PRICE_CUT_FORMULA_MPP =
      "IF(INT(G:G)-INT(L:L)=INT(G:G),\"0\",IF(INT(G:G)>=INT(L:L),INT(G:G)-INT(L:L),\"KESALAHAN !!! Harga Akhir Tidak Bisa Lebih Dari Harga Jual\"))";

  private static final String DASHOBOARD_ROW1_FORMULA = "=\"TotalProducts:\"&Other!$A3";
  private static final String DASHOBOARD_ROW2_FORMULA = "=\"TotalProducts:\"&Other!$C3";
  private static final int HEADER = 0;
  private static final int PRICE_CUT_COLUMN = 7;
  private static final int DISCOUNT_PERCENTAGE_COLUMN = 8;
  private static final int DISCOUNT_PERCENTAGE_COLUMN_MPP = 9;
  private static final int FINAL_PRICE_COLUMN = 11;
  private static final int FINAL_PRICE_COLUMN_MPP = 12;
  private static final String FINAL_PRICE_NAME = BulkParameters.HARGA_AKHIR;
  private static final int TITLE = 13;
  private static final int DASHOBOARD_LEFT = 14;
  private static final int DASHOBOARD_RIGHT = 15;
  private static final int INSTRUCTION_LEFT = 16;
  private static final int INSTRUCTION_RIGHT = 17;
  private static final int QUOTA_COLUMN = 12;
  private static final String QUOTA_COLUMN_NAME = BulkParameters.KUOTA;
  private static final int FIRST_COL_INDEX = 0;
  private static final int LAST_COL_INDEX = 12;
  private static final int LAST_COL_INDEX_MPP = 13;
  private static final int DEFAULT = 1;
  private static final String PERCENTAGE_FORMAT = "0%";
  private static final String NUMBER_FORMAT = "0";
  private static final String TITLE_FONT = "Arial";
  private static final int CONTENT_START_ROW = 7;
  private static final String LOW_PRICE_WARNING_THRESHOLD = "1000";
  private static final String HIGH_DISCOUNT_THRESHOLD = "0.8";
  private static final int DEFAULT_CELL_HEIGHT = 800;
  private static final String FS_RECOMMENDATION_YES = "YES";
  private static final String FS_RECOMMENDATION_NO = "NO";


  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private PromoAnalyticsOutboundService promoAnalyticsOutboundService;

  @Value("${multipickuppoint.workflow.enabled}")
  private boolean multiPickupPointEnabled;

  @Value("${pricing.multipickuppoint.enabled}")
  private boolean pricingMultiPickupPointEnabled;

  @Value("${pricing.campaign.recommendation.enabled}")
  private boolean pricingCampaignRecommendationEnabled;

  private static final List<String> HEADER_LIST_WITH_PICKUP_POINT =
      ImmutableList.<String>builder().add(BulkParameters.BLIBLI_SKU).
          add(BulkParameters.PICKUP_POINT_CODE).
          add(BulkParameters.PICKUP_POINT_NAME).
          add(BulkParameters.NAMA_PRODUK).
          add(BulkParameters.SKU_NAME).
          add(BulkParameters.HARGA_NORMAL).
          add(BulkParameters.HARGA_JUAL).
          add(BulkParameters.POTONGAN_HARGA).
          add(BulkParameters.PERSENTASE_DISKON).
          add(BulkParameters.STOK_TERSEDIA).
          add(BulkParameters.REKOMENDASI).
          add(BulkParameters.HARGA_AKHIR).
          add(BulkParameters.KUOTA).
          add(BulkParameters.KOMENTAR).build();

  private static final List<String> HEADER_LIST_WITH_PICKUP_POINT_CAMPAIGN_RECOMMENDATION =
    ImmutableList.<String>builder().add(BulkParameters.BLIBLI_SKU).
      add(BulkParameters.PICKUP_POINT_CODE).
      add(BulkParameters.PICKUP_POINT_NAME).
      add(BulkParameters.NAMA_PRODUK).
      add(BulkParameters.SKU_NAME).
      add(BulkParameters.HARGA_NORMAL).
      add(BulkParameters.HARGA_JUAL).
      add(BulkParameters.POTONGAN_HARGA).
      add(BulkParameters.PERSENTASE_DISKON).
      add(BulkParameters.STOK_TERSEDIA).
      add(BulkParameters.HARGA_REKOMENDASI).
      add(BulkParameters.REKOMENDASI).
      add(BulkParameters.HARGA_AKHIR).
      add(BulkParameters.KUOTA).
      add(BulkParameters.KOMENTAR).build();

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public byte[] generateCsvFile(BulkCsvModel csvModel, BulkDataResponse dataResponse)
      throws IllegalAccessException, NoSuchMethodException, InvocationTargetException, IOException {
    return super.generateCsvFile(csvModel, dataResponse);
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) {
    return pricingCampaignRecommendationEnabled ?
        HEADER_LIST_WITH_PICKUP_POINT_CAMPAIGN_RECOMMENDATION :
        HEADER_LIST_WITH_PICKUP_POINT;
  }

  @Override
  public Workbook generateDataSheet(List<String> headerList, List<List<String>> rowDataList,
      int bulkUpdateTemplateColumnWidth) {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet(BulkParameters.DATA_SHEET);
    ((SXSSFSheet) dataSheet).trackAllColumnsForAutoSizing();
    int rowId = addTitleAndWarningDashboard(workbook, dataSheet);
    dataSheet.createRow(rowId);
    rowId++;
    Row row;
    row = dataSheet.createRow((short) rowId);
    rowId++;
    int cellIndex = 0;
    CellStyle headerCellStyle = getCellStyle(workbook, HEADER);
    CellStyle priceCutCellStyle = getCellStyle(workbook, PRICE_CUT_COLUMN);
    CellStyle discountCellStyle = getCellStyle(workbook, DISCOUNT_PERCENTAGE_COLUMN);
    CellStyle finalPriceCellStyle = getCellStyle(workbook, FINAL_PRICE_COLUMN);
    CellStyle quotaPriceCellStyle = getCellStyle(workbook, QUOTA_COLUMN);
    CellStyle defaultPriceCellStyle = getCellStyle(workbook, DEFAULT);
    Map<Integer, String> columnToNameMap = new HashMap<>();
    for (String headers : headerList) {
      Cell cell = row.createCell((short) cellIndex);
      cellIndex++;
      columnToNameMap.put(cellIndex, headers);
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(headers);
      cell.setCellStyle(headerCellStyle);
    }
    for (List<String> rowData : rowDataList) {
      row = dataSheet.createRow(rowId);
      rowId++;
      int cellId = 0;
      for (String cellValue : rowData) {
        Cell cell = row.createCell(cellId);
        cellId++;
        String columnName = columnToNameMap.get(cellId);
        switch (columnName) {
          case BulkParameters.POTONGAN_HARGA: {
            if (pricingCampaignRecommendationEnabled) {
              cell.setCellFormula(PRICE_CUT_FORMULA_MPP_CAMPAIGN_RECOMMENDATION);
            } else {
              cell.setCellFormula(PRICE_CUT_FORMULA_MPP);
            }
            cell.setCellStyle(priceCutCellStyle);
            break;
          }
          case BulkParameters.PERSENTASE_DISKON:
            cell.setCellType(Cell.CELL_TYPE_FORMULA);
            if (pricingCampaignRecommendationEnabled) {
              cell.setCellFormula(DISCOUNT_PERCENTAGE_FORMULA_MPP_CAMPAIGN_RECOMMENDATION);
            } else {
              cell.setCellFormula(DISCOUNT_PERCENTAGE_FORMULA_MPP);
            }
            cell.setCellStyle(discountCellStyle);
            break;
          case FINAL_PRICE_NAME: {
            cell.setCellStyle(finalPriceCellStyle);
            break;
          }
          case QUOTA_COLUMN_NAME: {
            cell.setCellStyle(quotaPriceCellStyle);
            break;
          }
          default:
            cell.setCellValue(cellValue);
            cell.setCellStyle(defaultPriceCellStyle);
            break;
        }
      }
      if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
        row.createCell(cellId).setCellFormula(createConditionalCommentForRowMppTemplate(rowId));
      }
      else{
        row.createCell(cellId).setCellFormula(createConditionalCommentForRow(rowId));
      }
    }
    createOtherSheetForDashboardMetrics(workbook, dataSheet);
    addConditionFormatting(dataSheet);
    if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
      for (int i = 0; i < LAST_COL_INDEX_MPP; i++) {
        dataSheet.autoSizeColumn(i, true);
      }
      dataSheet.setColumnWidth(LAST_COL_INDEX_MPP, 20000);

    } else {
      for (int i = 0; i < LAST_COL_INDEX; i++) {
        dataSheet.autoSizeColumn(i, true);
      }
      dataSheet.setColumnWidth(LAST_COL_INDEX, 20000);
    }

    return workbook;
  }

  private static String createConditionalCommentForRow(int rowId) {
    return "IFERROR(IF(AND((H" + rowId + "*100)>80,K" + rowId
        + "<1000),\"Diskon di atas 80% and harga jual produk rendah. Silakan cek.\",IF" + "((H" + rowId
        + "*100)>80,\"Diskon di atas 80%. Silakan cek.\",IF((K" + rowId
        + ")<1000,\"Harga jual produk rendah. Silakan cek.\"," + "\" \"))),\"\")";
  }

  private static String createConditionalCommentForRowMppTemplate(int rowId) {
    return "IFERROR(IF(AND((I" + rowId + "*100)>80,L" + rowId
      + "<1000),\"Diskon di atas 80% and harga jual produk rendah. Silakan cek.\",IF" + "((I" + rowId
      + "*100)>80,\"Diskon di atas 80%. Silakan cek.\",IF((L" + rowId
      + ")<1000,\"Harga jual produk rendah. Silakan cek.\"," + "\" \"))),\"\")";
  }


  private void addConditionFormatting(Sheet dataSheet) {
    int rows = dataSheet.getPhysicalNumberOfRows();
    if (rows >= CONTENT_START_ROW) {
      SheetConditionalFormatting sheetCF1 = dataSheet.getSheetConditionalFormatting();
      SheetConditionalFormatting sheetCF2 = dataSheet.getSheetConditionalFormatting();
      ConditionalFormattingRule rule1 = sheetCF1.createConditionalFormattingRule(ComparisonOperator.EQUAL, "\"\"");
      injectColouredPatternFormat(rule1, IndexedColors.YELLOW.index);
      ConditionalFormattingRule rule2 =
          sheetCF1.createConditionalFormattingRule(ComparisonOperator.LT, LOW_PRICE_WARNING_THRESHOLD);
      injectColouredPatternFormat(rule2, IndexedColors.LIGHT_ORANGE.index);
      ConditionalFormattingRule rule3 = sheetCF2.createConditionalFormattingRule(ComparisonOperator.EQUAL, "\"-\"");
      injectColouredPatternFormat(rule3, IndexedColors.WHITE.index);
      ConditionalFormattingRule rule4 =
          sheetCF2.createConditionalFormattingRule(ComparisonOperator.GT, HIGH_DISCOUNT_THRESHOLD);
      injectColouredPatternFormat(rule4, IndexedColors.RED.index);
      if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
        sheetCF1.addConditionalFormatting(new CellRangeAddress[] {
          new CellRangeAddress(CONTENT_START_ROW - 1, rows - 1, FINAL_PRICE_COLUMN_MPP - 1,
            FINAL_PRICE_COLUMN_MPP - 1)}, rule1, rule2);
        sheetCF2.addConditionalFormatting(new CellRangeAddress[] {
          new CellRangeAddress(CONTENT_START_ROW - 1, rows - 1, DISCOUNT_PERCENTAGE_COLUMN_MPP - 1,
            DISCOUNT_PERCENTAGE_COLUMN_MPP - 1)}, rule3, rule4);
      } else {
        sheetCF1.addConditionalFormatting(new CellRangeAddress[] {
          new CellRangeAddress(CONTENT_START_ROW - 1, rows - 1, FINAL_PRICE_COLUMN - 1,
            FINAL_PRICE_COLUMN - 1)}, rule1, rule2);
        sheetCF2.addConditionalFormatting(new CellRangeAddress[] {
          new CellRangeAddress(CONTENT_START_ROW - 1, rows - 1, DISCOUNT_PERCENTAGE_COLUMN - 1,
             DISCOUNT_PERCENTAGE_COLUMN - 1)}, rule3, rule4);
      }
    }
  }

  private static void injectColouredPatternFormat(ConditionalFormattingRule rule, short colorIndex) {
    PatternFormatting pattermFmt = rule.createPatternFormatting();
    pattermFmt.setFillBackgroundColor(colorIndex);
    pattermFmt.setFillPattern(PatternFormatting.SOLID_FOREGROUND);
  }

  private int addTitleAndWarningDashboard(SXSSFWorkbook workbook, Sheet dataSheet) {
    int rowId = 0;
    rowId = createCellsAndSetData(workbook, dataSheet, TITLE, rowId, 0, BulkParameters.BULK_CAMPAIGN_TITLE, null,
        (short) 500, new CellRangeAddress(rowId, rowId, FIRST_COL_INDEX, LAST_COL_INDEX));

    rowId = createCellsAndSetData(workbook, dataSheet, DASHOBOARD_LEFT, rowId, 0, BulkParameters.DASHBOARD_ROW_1, null,
        (short) DEFAULT_CELL_HEIGHT, new CellRangeAddress(rowId, rowId, FIRST_COL_INDEX, FIRST_COL_INDEX + 3));

    rowId = createCellsAndSetData(workbook, dataSheet, DASHOBOARD_RIGHT, rowId - 1, 4, null, DASHOBOARD_ROW1_FORMULA,
        (short) DEFAULT_CELL_HEIGHT, new CellRangeAddress(rowId - 1, rowId - 1, FIRST_COL_INDEX + 4, FIRST_COL_INDEX + 5));

    rowId = createCellsAndSetData(workbook, dataSheet, DASHOBOARD_LEFT, rowId, 0, BulkParameters.DASHBOARD_ROW_2, null,
        (short) DEFAULT_CELL_HEIGHT, new CellRangeAddress(rowId, rowId, FIRST_COL_INDEX, FIRST_COL_INDEX + 3));

    rowId = createCellsAndSetData(workbook, dataSheet, DASHOBOARD_RIGHT, rowId - 1, 4, null, DASHOBOARD_ROW2_FORMULA,
        (short) DEFAULT_CELL_HEIGHT, new CellRangeAddress(rowId - 1, rowId - 1, FIRST_COL_INDEX + 4, FIRST_COL_INDEX + 5));

    rowId =
        createCellsAndSetData(workbook, dataSheet, INSTRUCTION_LEFT, rowId, 0, BulkParameters.BULK_CAMPAIGN_INSTRUCTION,
            null, (short) DEFAULT_CELL_HEIGHT, new CellRangeAddress(rowId, rowId, FIRST_COL_INDEX, FIRST_COL_INDEX + 2));

    rowId = createCellsAndSetData(workbook, dataSheet, INSTRUCTION_RIGHT, rowId - 1, 3, null, null, (short) DEFAULT_CELL_HEIGHT,
        new CellRangeAddress(rowId - 1, rowId - 1, FIRST_COL_INDEX + 3, FIRST_COL_INDEX + 5));
    return rowId;
  }

  private int createCellsAndSetData(SXSSFWorkbook workbook, Sheet dataSheet, int styleIndex, int rowId, int cellId,
      String value, String formula, short height, CellRangeAddress cellRangeAddress) {
    CellStyle cellStyle = getCellStyle(workbook, styleIndex);
    Row row = dataSheet.getRow(rowId);
    if (Objects.isNull(row)) {
      row = dataSheet.createRow(rowId);
      row.setHeight(height);
    }
    Cell cell = row.createCell(cellId);
    cell.setCellStyle(cellStyle);
    Optional.ofNullable(value).ifPresent(cell::setCellValue);
    Optional.ofNullable(formula).ifPresent(cell::setCellFormula);
    dataSheet.addMergedRegion(cellRangeAddress);
    return rowId + 1;
  }

  private void createOtherSheetForDashboardMetrics(SXSSFWorkbook workbook, Sheet dataSheet) {
    Sheet otherSheet = workbook.createSheet("Other");
    otherSheet.createRow(1).createCell(0).setCellValue(BulkParameters.HIGH_DISCOUNT);
    otherSheet.getRow(1).createCell(2).setCellValue(BulkParameters.LOW_PRICE);
    int rows = dataSheet.getPhysicalNumberOfRows();
    CellStyle style = getCellStyle(workbook, PRICE_CUT_COLUMN);
    style.setAlignment(HorizontalAlignment.CENTER);
    IntStream.of(0, 2).forEach(cellId -> {
      Cell cell;
      if (Objects.nonNull(otherSheet.getRow(2))) {
        cell = otherSheet.getRow(2).createCell(cellId);
      }
      else {
        cell = otherSheet.createRow(2).createCell(cellId);
      }
      cell.setCellStyle(style);
      if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
        cell.setCellFormula(getMetricCountFormulaMPP(cellId, rows));
      } else {
        cell.setCellFormula(getMetricCountFormula(cellId, rows));
      }
    });
  }

  private static String getMetricCountFormula(int cellId, int rows) {
    char column = (char) ('G' + cellId);
    if (cellId == 0) {
      return "COUNTIFS(Data!$" + column + "$" + CONTENT_START_ROW + ":$" + column + "$" + rows + ",\">"
          + HIGH_DISCOUNT_THRESHOLD + "\")";
    } else {
      return "COUNTIFS(Data!$" + column + "$" + CONTENT_START_ROW + ":$" + column + "$" + rows + ",\"<"
          + LOW_PRICE_WARNING_THRESHOLD + "\")";
    }
  }

  private static String getMetricCountFormulaMPP(int cellId, int rows) {
    char column = (char) ('I' + cellId);
    if (cellId == 0) {
      return "COUNTIFS(Data!$" + column + "$" + CONTENT_START_ROW + ":$" + column + "$" + rows + ",\">"
        + HIGH_DISCOUNT_THRESHOLD + "\")";
    } else {
      column = 'M';
      return "COUNTIFS(Data!$" + column + "$" + CONTENT_START_ROW + ":$" + column + "$" + rows + ",\"<"
        + LOW_PRICE_WARNING_THRESHOLD + "\")";
    }
  }

  private CellStyle getCellStyle(SXSSFWorkbook workbook, int columnNumber) {
    CellStyle style = workbook.createCellStyle();
    switch (columnNumber) {
      case HEADER: {
        addBackgroundColour(style, IndexedColors.LIGHT_GREEN.getIndex());
        addBorderForCell(style);
        break;
      }
      case PRICE_CUT_COLUMN: {
        style.setDataFormat(workbook.createDataFormat().getFormat(NUMBER_FORMAT));
        addBorderForCell(style);
        break;
      }
      case DISCOUNT_PERCENTAGE_COLUMN: {
        style.setDataFormat(workbook.createDataFormat().getFormat(PERCENTAGE_FORMAT));
        addBorderForCell(style);
        break;
      }
      case FINAL_PRICE_COLUMN: {
        setBorderAndBackgroundColourForFinalPrice(workbook, style);
        break;
      }
      case QUOTA_COLUMN: {
        addBackgroundColour(style, IndexedColors.YELLOW.getIndex());
        addBorderForCell(style);
        break;
      }
      case TITLE: {
        addBackgroundColour(style, IndexedColors.SKY_BLUE.getIndex());
        addTitleFont(workbook, style);
        break;
      }
      case DASHOBOARD_LEFT: {
        setBackgroundColourAndDashboardFont(style, IndexedColors.BLACK, workbook);
        style.setWrapText(true);
        break;
      }
      case DASHOBOARD_RIGHT: {
        setBackgroundColourAndDashboardFont(style, IndexedColors.GREY_40_PERCENT, workbook);
        style.setAlignment(HorizontalAlignment.CENTER);
        break;
      }
      case INSTRUCTION_LEFT: {
        setBackgroundColourAndDashboardFont(style, IndexedColors.LIGHT_ORANGE, workbook);
        style.setWrapText(true);
        break;
      }
      case INSTRUCTION_RIGHT: {
        setBackgroundColourAndDashboardFont(style, IndexedColors.LIGHT_ORANGE, workbook);
        break;
      }
      default: {
        addBorderForCell(style);
        break;
      }

    }
    return style;
  }

  private void setBackgroundColourAndDashboardFont(CellStyle style, IndexedColors grey40Percent,
      SXSSFWorkbook workbook) {
    addBackgroundColour(style, grey40Percent.getIndex());
    addDashboardFont(workbook, style);
  }

  private void setBorderAndBackgroundColourForFinalPrice(SXSSFWorkbook workbook, CellStyle style) {
    addBackgroundColour(style, IndexedColors.YELLOW.getIndex());
    addBorderForCell(style);
    style.setDataFormat(workbook.createDataFormat().getFormat(NUMBER_FORMAT));
  }

  private void addDashboardFont(SXSSFWorkbook workbook, CellStyle style) {
    Font font = workbook.createFont();
    font.setColor(IndexedColors.WHITE.index);
    font.setFontHeightInPoints((short) 12);
    style.setFont(font);
  }

  private void addTitleFont(SXSSFWorkbook workbook, CellStyle style) {
    Font font = workbook.createFont();
    font.setFontHeightInPoints((short) 14);
    font.setFontName(TITLE_FONT);
    font.setBold(true);
    font.setItalic(true);
    font.setColor(IndexedColors.WHITE.index);
    style.setFont(font);
  }

  private void addBackgroundColour(CellStyle backgroundStyle, short colourIndex) {
    backgroundStyle.setFillForegroundColor(colourIndex);
    backgroundStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
  }

  private void addBorderForCell(CellStyle borderStyle) {
    borderStyle.setBorderBottom(BorderStyle.THIN);
    borderStyle.setBottomBorderColor(IndexedColors.BLACK.getIndex());
    borderStyle.setBorderLeft(BorderStyle.THIN);
    borderStyle.setLeftBorderColor(IndexedColors.BLACK.getIndex());
    borderStyle.setBorderRight(BorderStyle.THIN);
    borderStyle.setRightBorderColor(IndexedColors.BLACK.getIndex());
    borderStyle.setBorderTop(BorderStyle.THIN);
    borderStyle.setTopBorderColor(IndexedColors.BLACK.getIndex());
  }

  @Override
  public void generateFile(String filepath, byte[] content) throws IOException {
    super.generateFile(filepath, content);
  }

  @Override
  public String getUserName(String email) {
    return super.getUserName(email);
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    BulkCampaignProductResponse bulkCampaignProductResponse = (BulkCampaignProductResponse) response;
    List<List<String>> rowData = new ArrayList<>();
    int pricingApiBatchSize = Integer.valueOf(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.PRICING_API_BATCH_SIZE).getValue());
    LowestPriceRecommendationRequest recommendationRequest =
        LowestPriceRecommendationRequest.builder().recommendedWeek(bulkCampaignProductResponse.getRecommendedWeek())
            .promoType(bulkCampaignProductResponse.getPromoType()).build();
    Map<String, LowestPriceResponse> itemSkuLowestPriceMap = new HashMap<>();
      List<List<ProductLevel3SummaryResponse>> summaryResponsesPartition = Lists.partition(
          bulkCampaignProductResponse.getProductLevel3SummaryResponses(), pricingApiBatchSize);
      for (List<ProductLevel3SummaryResponse> summaryResponsesList : summaryResponsesPartition) {
        List<ItemInfoDto> itemInfoDtos = summaryResponsesList.stream().map(summaryResponse -> new ItemInfoDto(summaryResponse.getItemSku(),
            summaryResponse.getPickupPointCode())).collect(Collectors.toList());
        recommendationRequest.setItemInfo(itemInfoDtos);
        LowestPriceRecommendationResponse recommendationResponse =
            promoAnalyticsOutboundService.getLowestPriceRecommendationV2(recommendationRequest);
        if (Objects.nonNull(recommendationResponse))
          itemSkuLowestPriceMap.putAll(recommendationResponse.getItemSkuLowestPriceMap());
      }
      for (ProductLevel3SummaryResponse productLevel3SummaryResponse : bulkCampaignProductResponse.getProductLevel3SummaryResponses()) {
        rowData.add(getContents(productLevel3SummaryResponse, itemSkuLowestPriceMap.get(productLevel3SummaryResponse.getItemSku()
            + Constant.HYPHEN + productLevel3SummaryResponse.getPickupPointCode())));
      }
    return rowData;
  }

  // Adding empty data for last two columns since they are user input fields and,
  // while generation of workbook styling gets applied to them
  private List<String> getContents(ProductLevel3SummaryResponse productLevel3SummaryResponse,
      LowestPriceResponse lowestPriceResponse) {
    List<String> rowDatum = new ArrayList<>();
    rowDatum.add(productLevel3SummaryResponse.getItemSku());
    if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
      rowDatum.add(productLevel3SummaryResponse.getPickupPointCode());
      rowDatum.add(productLevel3SummaryResponse.getPickupPointName());
    }
    rowDatum.add(productLevel3SummaryResponse.getProductName());
    rowDatum.add(productLevel3SummaryResponse.getItemName());
    rowDatum.add(String.format("%.0f", productLevel3SummaryResponse.getPrices().get(0).getPrice()));
    rowDatum.add(String.format("%.0f", productLevel3SummaryResponse.getPrices().get(0).getSalePrice()));
    rowDatum.add(String.valueOf(0));
    rowDatum.add(String.valueOf(0));
    if (productLevel3SummaryResponse.getSynchronizeStock())
      rowDatum.add(String.valueOf(productLevel3SummaryResponse.getAvailableStockLevel1()));
    else
      rowDatum.add(String.valueOf(productLevel3SummaryResponse.getAvailableStockLevel2()));
    if (Objects.nonNull(lowestPriceResponse) && (Double.compare(lowestPriceResponse.getLowestPrice(),
        Optional.ofNullable(productLevel3SummaryResponse.getPrices().get(0).getSalePrice()).orElse(Double.MIN_NORMAL)))
        < 0) {
      rowDatum.add(Constant.LOWER_THAN_SYMBOL + lowestPriceResponse.getLowestPrice());
    } else {
      rowDatum.add(StringUtils.EMPTY);
    }
    if (pricingCampaignRecommendationEnabled && Objects.nonNull(lowestPriceResponse)) {
      rowDatum.add(lowestPriceResponse.isFsRecommended() ? FS_RECOMMENDATION_YES : FS_RECOMMENDATION_NO);
    }
    rowDatum.add(StringUtils.EMPTY);
    rowDatum.add(StringUtils.EMPTY);
    return rowDatum;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.CAMPAIGN_PRODUCT) + request.getRequestId();
  }

  @Override
  public String getFilePath(String directoryPath, String filename) {
    return super.getFilePath(directoryPath, filename);
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put("name", getUserName(request.getUsername()));
    emailParameters.put("reqId", request.getRequestId());
    emailParameters.put("businessPartnerCode", request.getMerchantId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM,
        MessageUtil.getMessage(EmailConstants.BULK_CAMPAIGN_PRODUCT_DOWNLOAD_TEMPLATE_ID, lang));
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, EmailConstants.CAMPAIGN_PRODUCT_SUBJECT);
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BulkCampaignProductResponse bulkCampaignProductResponse = (BulkCampaignProductResponse) response;
    return bulkCampaignProductResponse.getProductLevel3SummaryResponses().size();
  }
}
