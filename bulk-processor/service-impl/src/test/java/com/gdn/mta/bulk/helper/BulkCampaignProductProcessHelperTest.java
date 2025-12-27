package com.gdn.mta.bulk.helper;

import static com.gdn.mta.bulk.SystemParameterConfigNames.PRICING_API_BATCH_SIZE;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;

import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkCampaignProductResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.service.PromoAnalyticsOutboundService;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.partners.promo.analytics.web.model.request.LowestPriceRecommendationRequest;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceRecommendationResponse;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceResponse;
import com.google.common.collect.ImmutableList;

public class BulkCampaignProductProcessHelperTest {

  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BulkParameters.BLIBLI_SKU).add(BulkParameters.NAMA_PRODUK).
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

  private static final String PRICE_CUT_FORMULA =
      "IF(INT(E:E)-INT(J:J)=INT(E:E),\"0\",IF(INT(E:E)>=INT(J:J),INT(E:E)-INT(J:J),\"KESALAHAN !!! Harga Akhir Tidak Bisa Lebih Dari Harga Jual\"))";

  private static final String PRICE_CUT_FORMULA_MPP_CAMPAIGN_RECOMMENDATION =
      "IF(INT(G:G)-INT(M:M)=INT(G:G),\"0\",IF(INT(G:G)>=INT(M:M),INT(G:G)-INT(M:M),\"KESALAHAN !!! Harga Akhir Tidak Bisa Lebih Dari Harga Jual\"))";
  private static final String DISCOUNT_PERCENTAGE_FORMULA =
      "IF(INT(J:J)>INT(E:E), \"KESALAHAN !!! Harga Akhir Tidak Bisa Lebih Dari Harga Jual\", IF(INT(F:F)/INT(E:E)=0,"
          + "\"-\",(INT(F:F)/INT(E:E))*100%))";

  private static final String DISCOUNT_PERCENTAGE_FORMULA_MPP_CAMPAIGN_RECOMMENDATION =
      "IF(INT(M:M)>INT(G:G), \"KESALAHAN !!! Harga Akhir Tidak Bisa Lebih Dari Harga Jual\", IF(INT(H:H)/INT(G:G)=0,\"-\",(INT(H:H)/INT(G:G))*100%))";

  private static final String DISCOUNT_PERCENTAGE_FORMULA_MPP =
      "IF(INT(L:L)>INT(G:G), \"KESALAHAN !!! Harga Akhir Tidak Bisa Lebih Dari Harga Jual\", IF(INT(H:H)/INT(G:G)=0,\"-\",(INT(H:H)/INT(G:G))*100%))";
  private static final String PRICE_CUT_FORMULA_MPP =
      "IF(INT(G:G)-INT(L:L)=INT(G:G),\"0\",IF(INT(G:G)>=INT(L:L),INT(G:G)-INT(L:L),\"KESALAHAN !!! Harga Akhir Tidak Bisa Lebih Dari Harga Jual\"))";

  private static final
  String DASHOBOARD_ROW1_FORMULA = "=\"TotalProducts:\"&Other!$A3";
  private static final String DASHOBOARD_ROW2_FORMULA = "=\"TotalProducts:\"&Other!$C3";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_NAME = "pickupPointName";

  private static final String DEFAULT_REQUEST_ID = "REQUEST ID";
  private static final String OFFLINE_ORDER_TYPE = "OFF2ON";
  private static final String PERCENTAGE_FORMAT = "0%";
  private static final String STORE_ID = "10001";
  private static final String ITEM_SKU = "itemSku";
  private static final String NUMBER_FORMAT = "0";
  private static final byte[] CONTENT = new byte[100];
  private static final String FILEPATH = "Test";
  private static final double LARGE_VALUE = 9223372036854776000.0;
  private static final int LAST_COL_INDEX = 11;
  private static final int CONTENT_START_ROW = 7;
  private static List<String> rowData;
  private static List<String> rowDataMpp;

  private OrderItemSummaryRequest orderItemSummaryRequest;
  private OrderItemSummaryRequest orderItemSummaryRequestOffline;
  ProductLevel3SummaryResponse productLevel3SummaryResponse;

  @InjectMocks
  private BulkCampaignProductProcessHelper bulkCampaignProductProcessHelper;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private PromoAnalyticsOutboundService promoAnalyticsOutboundService;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
    productLevel3SummaryResponse.setPrices(Collections.singletonList(productLevel3PriceResponse));
    productLevel3SummaryResponse.setAvailableStockLevel1(10);
    productLevel3SummaryResponse.setAvailableStockLevel2(1);
    productLevel3SummaryResponse.setSynchronizeStock(true);
    productLevel3SummaryResponse.setItemSku(ITEM_SKU);

    rowData = new ArrayList<>();
    rowData.add("Blibli-Sku");
    rowData.add("ProductName");
    rowData.add("ItemName");
    rowData.add(String.valueOf(Double.parseDouble("9223372036854776000")));
    rowData.add(String.valueOf(Double.parseDouble("9223372036854776000")));
    rowData.add(String.valueOf(15));
    rowData.add(String.valueOf(100));
    rowData.add(String.valueOf(100));
    rowData.add(String.valueOf(100));
    rowData.add(String.valueOf(100));
    rowData.add("<" + 23);
    rowDataMpp = new ArrayList<>();
    rowDataMpp.add("Blibli-Sku");
    rowDataMpp.add("PP code");
    rowDataMpp.add(PICKUP_POINT_NAME);
    rowDataMpp.add("ProductName");
    rowDataMpp.add("ItemName");
    rowDataMpp.add(String.valueOf(Double.parseDouble("9223372036854776000")));
    rowDataMpp.add(String.valueOf(Double.parseDouble("9223372036854776000")));
    rowDataMpp.add(String.valueOf(15));
    rowDataMpp.add(String.valueOf(100));
    rowDataMpp.add(String.valueOf(100));
    rowDataMpp.add(String.valueOf(100));
    rowDataMpp.add(String.valueOf(100));
    rowDataMpp.add("<" + 23);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(String.valueOf(25));
    when(this.systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, PRICING_API_BATCH_SIZE))
        .thenReturn(systemParameterConfig);
    LowestPriceRecommendationRequest lowestPriceRecommendationRequest =
        LowestPriceRecommendationRequest.builder().itemSkus(new HashSet<>()).build();
    when(promoAnalyticsOutboundService.getLowestPriceRecommendation(lowestPriceRecommendationRequest))
        .thenReturn(new LowestPriceRecommendationResponse());
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper, "multiPickupPointEnabled",
      false);
  }

  @Test
  public void getCsvHeadersMapTest() throws Exception {
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    BulkCsvModel bulkCsvModel = this.bulkCampaignProductProcessHelper.getCsvHeadersMap(bulkDownloadRequest);
    Assertions.assertNull(bulkCsvModel);
  }

  @Test
  public void generateCsvFileTest() throws  Exception {
    BulkCsvModel bulkCsvModels = new BulkCsvModel(new ArrayList<>(), new HashMap<>());
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    byte[] bulkCsvModel = this.bulkCampaignProductProcessHelper.generateCsvFile(bulkCsvModels, bulkDataResponse);
    assertNotNull(bulkCsvModel);
  }

  @Test
  public void getHeaderListTest() throws  Exception {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    List<String> response = this.bulkCampaignProductProcessHelper.getHeaderList(bulkDataResponse);
    assertNotNull(response);
  }

  @Test
  public void getHeaderList_campaignSwitchOnTest() throws  Exception {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper,"pricingCampaignRecommendationEnabled",true);
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    List<String> response = this.bulkCampaignProductProcessHelper.getHeaderList(bulkDataResponse);
    Assertions.assertTrue(response.contains(BulkParameters.PICKUP_POINT_CODE));
  }

  @Test
  public void getHeaderList_PricingmppSwitchOnTest() throws  Exception {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper, "pricingMultiPickupPointEnabled",
        true);
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    List<String> response = this.bulkCampaignProductProcessHelper.getHeaderList(bulkDataResponse);
    Assertions.assertTrue(response.contains(BulkParameters.PICKUP_POINT_CODE));
  }

  @Test
  public void generateDataSheetTest() throws  Exception {
    List<String> headers = new ArrayList<>();
    List<List<String>> rows = new ArrayList<>();
    Workbook response = this.bulkCampaignProductProcessHelper.generateDataSheet(headers, rows, 1);
    assertNotNull(response);
  }

  @Test
  public void generateDataSheetXCampTest() {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper,"pricingCampaignRecommendationEnabled",true);
    List<List<String>> rows = new ArrayList<>();
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    CellStyle discountCellStyle = workbook.createCellStyle();
    CellStyle finalPriceCellStyle = workbook.createCellStyle();
    getDiscountCellStyle(discountCellStyle, workbook);
    getFinalPriceCellStyle(finalPriceCellStyle, workbook);
    rows.add(rowData);
    rows.add(rowData);
    Workbook response = this.bulkCampaignProductProcessHelper.generateDataSheet(HEADER_LIST, rows, 1);
    Assertions.assertEquals(
        PRICE_CUT_FORMULA_MPP_CAMPAIGN_RECOMMENDATION, response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(5).getCellFormula());
    Assertions.assertEquals(DISCOUNT_PERCENTAGE_FORMULA_MPP_CAMPAIGN_RECOMMENDATION,
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(6).getCellFormula());
    Assertions.assertEquals("IFERROR(IF(AND((H7*100)>80,K7<1000),\"Diskon di atas 80% and harga jual produk rendah. Silakan cek"
            + ".\",IF((H7*100)>80,\"Diskon di atas 80%. Silakan cek.\",IF((K7)<1000,\"Harga jual produk rendah. "
            + "Silakan cek.\",\" \"))),\"\")",
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(LAST_COL_INDEX).getCellFormula());
    Assertions.assertEquals(DASHOBOARD_ROW1_FORMULA, response.getSheetAt(0).getRow(1).getCell(4).getCellFormula());
    Assertions.assertEquals(DASHOBOARD_ROW2_FORMULA, response.getSheetAt(0).getRow(2).getCell(4).getCellFormula());
    Assertions.assertEquals(ReflectionTestUtils.invokeMethod(bulkCampaignProductProcessHelper, "getMetricCountFormula", 0,
        response.getSheetAt(0).getPhysicalNumberOfRows()),
        response.getSheetAt(1).getRow(2).getCell(0).getCellFormula());
    Assertions.assertEquals(ReflectionTestUtils.invokeMethod(bulkCampaignProductProcessHelper, "getMetricCountFormula", 2,
        response.getSheetAt(0).getPhysicalNumberOfRows()),
        response.getSheetAt(1).getRow(2).getCell(2).getCellFormula());
    Assertions.assertEquals(discountCellStyle, response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(6).getCellStyle());
    Assertions.assertEquals(finalPriceCellStyle.getFillBackgroundColor(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(7).getCellStyle().getFillBackgroundColor());
    Assertions.assertEquals(finalPriceCellStyle.getFillPattern(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(9).getCellStyle().getFillPattern());
    Assertions.assertEquals(finalPriceCellStyle.getDataFormat(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(9).getCellStyle().getDataFormat());
    Assertions.assertEquals(String.valueOf(LARGE_VALUE),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(3).getStringCellValue());
    Assertions.assertEquals(String.valueOf(LARGE_VALUE),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(3).getStringCellValue());
    Assertions.assertEquals(IndexedColors.LIGHT_ORANGE.index,
        response.getSheetAt(0).getSheetConditionalFormatting().getConditionalFormattingAt(0).getRule(1)
            .getPatternFormatting().getFillBackgroundColor());
    assertNotNull(response);
  }

  @Test
  public void generateDataSheetXCampMppTrueTest() {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper,"pricingCampaignRecommendationEnabled",true);
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper, "multiPickupPointEnabled", false);
    List<List<String>> rows = new ArrayList<>();
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    CellStyle discountCellStyle = workbook.createCellStyle();
    CellStyle finalPriceCellStyle = workbook.createCellStyle();
    getDiscountCellStyle(discountCellStyle, workbook);
    getFinalPriceCellStyle(finalPriceCellStyle, workbook);
    rows.add(rowDataMpp);
    rows.add(rowDataMpp);
    Workbook response = this.bulkCampaignProductProcessHelper.generateDataSheet(HEADER_LIST_WITH_PICKUP_POINT, rows, 1);
    Assertions.assertEquals(
        PRICE_CUT_FORMULA_MPP_CAMPAIGN_RECOMMENDATION, response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(7).getCellFormula());
    Assertions.assertEquals(DISCOUNT_PERCENTAGE_FORMULA_MPP_CAMPAIGN_RECOMMENDATION,
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(8).getCellFormula());
    Assertions.assertEquals(
        "IFERROR(IF(AND((H7*100)>80,K7<1000),\"Diskon di atas 80% and harga jual produk rendah. Silakan cek." +
            "\",IF((H7*100)>80,\"Diskon di atas 80%. Silakan cek.\",IF((K7)<1000," +
            "\"Harga jual produk rendah. Silakan cek.\",\" \"))),\"\")",
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(13).getCellFormula());
    Assertions.assertEquals(DASHOBOARD_ROW1_FORMULA, response.getSheetAt(0).getRow(1).getCell(4).getCellFormula());
    Assertions.assertEquals(DASHOBOARD_ROW2_FORMULA, response.getSheetAt(0).getRow(2).getCell(4).getCellFormula());
    Assertions.assertEquals(ReflectionTestUtils.invokeMethod(bulkCampaignProductProcessHelper, "getMetricCountFormula", 0,
            response.getSheetAt(0).getPhysicalNumberOfRows()),
        response.getSheetAt(1).getRow(2).getCell(0).getCellFormula());
    Assertions.assertEquals(ReflectionTestUtils.invokeMethod(bulkCampaignProductProcessHelper, "getMetricCountFormula", 2,
            response.getSheetAt(0).getPhysicalNumberOfRows()),
        response.getSheetAt(1).getRow(2).getCell(2).getCellFormula());
    Assertions.assertEquals(discountCellStyle, response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(8).getCellStyle());
    Assertions.assertEquals(finalPriceCellStyle.getFillBackgroundColor(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(9).getCellStyle().getFillBackgroundColor());
    Assertions.assertEquals(finalPriceCellStyle.getFillPattern(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(11).getCellStyle().getFillPattern());
    Assertions.assertEquals(finalPriceCellStyle.getDataFormat(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(11).getCellStyle().getDataFormat());
    Assertions.assertEquals(String.valueOf(LARGE_VALUE),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(5).getStringCellValue());
    Assertions.assertEquals(String.valueOf(LARGE_VALUE),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(5).getStringCellValue());
    Assertions.assertEquals(PICKUP_POINT_NAME,
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(2).getStringCellValue());
    Assertions.assertEquals(IndexedColors.LIGHT_ORANGE.index,
        response.getSheetAt(0).getSheetConditionalFormatting().getConditionalFormattingAt(0).getRule(1)
            .getPatternFormatting().getFillBackgroundColor());
    assertNotNull(response);
  }

  @Test
  public void generateDataSheetMPPTest() {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper,"pricingCampaignRecommendationEnabled",true);
    List<List<String>> rows = new ArrayList<>();
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper, "multiPickupPointEnabled",
        true);
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    CellStyle discountCellStyle = workbook.createCellStyle();
    CellStyle finalPriceCellStyle = workbook.createCellStyle();
    getDiscountCellStyle(discountCellStyle, workbook);
    getFinalPriceCellStyle(finalPriceCellStyle, workbook);
    rows.add(rowData);
    rows.add(rowData);
    Workbook response = this.bulkCampaignProductProcessHelper.generateDataSheet(HEADER_LIST, rows, 1);
    Assertions.assertEquals(
        PRICE_CUT_FORMULA_MPP_CAMPAIGN_RECOMMENDATION, response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(5).getCellFormula());
    Assertions.assertEquals(DISCOUNT_PERCENTAGE_FORMULA_MPP_CAMPAIGN_RECOMMENDATION,
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(6).getCellFormula());
    Assertions.assertEquals("IFERROR(IF(AND((I7*100)>80,L7<1000),\"Diskon di atas 80% " +
            "and harga jual produk rendah. Silakan cek." +
            "\",IF((I7*100)>80,\"Diskon di atas 80%. Silakan cek.\",IF((L7)<1000," +
            "\"Harga jual produk rendah. Silakan cek.\",\" \"))),\"\")",
      response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(LAST_COL_INDEX)
        .getCellFormula());
    Assertions.assertEquals(DASHOBOARD_ROW1_FORMULA, response.getSheetAt(0).getRow(1).getCell(4).getCellFormula());
    Assertions.assertEquals(DASHOBOARD_ROW2_FORMULA, response.getSheetAt(0).getRow(2).getCell(4).getCellFormula());
    Assertions.assertEquals(ReflectionTestUtils.invokeMethod(bulkCampaignProductProcessHelper, "getMetricCountFormulaMPP", 0,
            response.getSheetAt(0).getPhysicalNumberOfRows()),
        response.getSheetAt(1).getRow(2).getCell(0).getCellFormula());
    Assertions.assertEquals(ReflectionTestUtils.invokeMethod(bulkCampaignProductProcessHelper, "getMetricCountFormulaMPP", 2,
            response.getSheetAt(0).getPhysicalNumberOfRows()),
        response.getSheetAt(1).getRow(2).getCell(2).getCellFormula());
    Assertions.assertEquals(discountCellStyle, response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(6).getCellStyle());
    Assertions.assertEquals(finalPriceCellStyle.getFillBackgroundColor(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(7).getCellStyle().getFillBackgroundColor());
    Assertions.assertEquals(finalPriceCellStyle.getFillPattern(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(9).getCellStyle().getFillPattern());
    Assertions.assertEquals(finalPriceCellStyle.getDataFormat(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(9).getCellStyle().getDataFormat());
    Assertions.assertEquals(String.valueOf(LARGE_VALUE),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(3).getStringCellValue());
    Assertions.assertEquals(String.valueOf(LARGE_VALUE),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(3).getStringCellValue());
    Assertions.assertEquals(IndexedColors.LIGHT_ORANGE.index,
        response.getSheetAt(0).getSheetConditionalFormatting().getConditionalFormattingAt(0).getRule(1)
            .getPatternFormatting().getFillBackgroundColor());
    assertNotNull(response);
  }

  @Test
  public void generateDataSheetPricingMPPTest() {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper,"pricingCampaignRecommendationEnabled",false);
    List<List<String>> rows = new ArrayList<>();
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper, "pricingMultiPickupPointEnabled",
        true);
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    CellStyle discountCellStyle = workbook.createCellStyle();
    CellStyle finalPriceCellStyle = workbook.createCellStyle();
    getDiscountCellStyle(discountCellStyle, workbook);
    getFinalPriceCellStyle(finalPriceCellStyle, workbook);
    rows.add(rowData);
    rows.add(rowData);
    Workbook response = this.bulkCampaignProductProcessHelper.generateDataSheet(HEADER_LIST, rows, 1);
    Assertions.assertEquals(
        PRICE_CUT_FORMULA_MPP, response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(5).getCellFormula());
    Assertions.assertEquals(DISCOUNT_PERCENTAGE_FORMULA_MPP,
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(6).getCellFormula());
    Assertions.assertEquals("IFERROR(IF(AND((I7*100)>80,L7<1000),\"Diskon di atas 80% and harga jual produk "
            + "rendah. Silakan cek"
            + ".\",IF((I7*100)>80,\"Diskon di atas 80%. Silakan cek.\",IF((L7)<1000,\"Harga jual "
            + "produk rendah. " + "Silakan cek.\",\" \"))),\"\")",
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(LAST_COL_INDEX)
            .getCellFormula());
    Assertions.assertEquals(DASHOBOARD_ROW1_FORMULA, response.getSheetAt(0).getRow(1).getCell(4).getCellFormula());
    Assertions.assertEquals(DASHOBOARD_ROW2_FORMULA, response.getSheetAt(0).getRow(2).getCell(4).getCellFormula());
    Assertions.assertEquals(ReflectionTestUtils.invokeMethod(bulkCampaignProductProcessHelper, "getMetricCountFormulaMPP", 0,
        response.getSheetAt(0).getPhysicalNumberOfRows()),
        response.getSheetAt(1).getRow(2).getCell(0).getCellFormula());
    Assertions.assertEquals(ReflectionTestUtils.invokeMethod(bulkCampaignProductProcessHelper, "getMetricCountFormulaMPP", 2,
        response.getSheetAt(0).getPhysicalNumberOfRows()),
        response.getSheetAt(1).getRow(2).getCell(2).getCellFormula());
    Assertions.assertEquals(discountCellStyle, response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(6).getCellStyle());
    Assertions.assertEquals(finalPriceCellStyle.getFillBackgroundColor(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(7).getCellStyle().getFillBackgroundColor());
    Assertions.assertEquals(finalPriceCellStyle.getFillPattern(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(9).getCellStyle().getFillPattern());
    Assertions.assertEquals(finalPriceCellStyle.getDataFormat(),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(9).getCellStyle().getDataFormat());
    Assertions.assertEquals(String.valueOf(LARGE_VALUE),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(3).getStringCellValue());
    Assertions.assertEquals(String.valueOf(LARGE_VALUE),
        response.getSheetAt(0).getRow(CONTENT_START_ROW - 1).getCell(3).getStringCellValue());
    Assertions.assertEquals(IndexedColors.LIGHT_ORANGE.index,
        response.getSheetAt(0).getSheetConditionalFormatting().getConditionalFormattingAt(0).getRule(1)
            .getPatternFormatting().getFillBackgroundColor());
    assertNotNull(response);
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

  private void getDiscountCellStyle(CellStyle cellStyle, Workbook workbook) {
    cellStyle.setDataFormat(workbook.createDataFormat().getFormat(PERCENTAGE_FORMAT));
    addBorderForCell(cellStyle);
  }

  private void getFinalPriceCellStyle(CellStyle style, Workbook workbook) {
    addBackgroundColour(style, IndexedColors.YELLOW.getIndex());
    addBorderForCell(style);
    style.setDataFormat(workbook.createDataFormat().getFormat(NUMBER_FORMAT));
  }

  private void addBackgroundColour(CellStyle backgroundStyle, short colourIndex) {
    backgroundStyle.setFillForegroundColor(colourIndex);
    backgroundStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
  }

  @Test
  public void getUserNameTest() throws  Exception {
    String response = this.bulkCampaignProductProcessHelper.getUserName("target");
    assertNotNull(response);
  }

  @Test
  public void modifyWorkbookTest() throws Exception {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    Workbook workbook = mock(Workbook.class);
    Workbook response = this.bulkCampaignProductProcessHelper.modifyWorkbook(workbook, bulkDataResponse);
  }


  @Test
  public void getRowDataTest_withRecommedation() throws Exception {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper,"pricingCampaignRecommendationEnabled",true);
    productLevel3SummaryResponse.getPrices().get(0).setSalePrice(11.0);
    productLevel3SummaryResponse.setPickupPointCode(PICKUP_POINT_CODE);
    LowestPriceRecommendationRequest lowestPriceRecommendationRequest =
        LowestPriceRecommendationRequest.builder().itemSkus(new HashSet<>(Arrays.asList(ITEM_SKU))).build();
    Map<String, LowestPriceResponse> map = new HashMap<>();
    map.put("itemSku-pickupPointCode", new LowestPriceResponse(10.0, true));
    when(promoAnalyticsOutboundService.getLowestPriceRecommendationV2(Mockito.any())).thenReturn(
        new LowestPriceRecommendationResponse(map));
    ArrayList<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    BulkCampaignProductResponse bulkDataResponse = new BulkCampaignProductResponse(productLevel3SummaryResponses);
    List<List<String>> row = this.bulkCampaignProductProcessHelper.getRowData(bulkDataResponse);
    assertNotNull(row);
    Assertions.assertEquals("YES", row.get(0).get(9));
  }

  @Test
  public void getRowDataTest_withRecommedationForFsRecommendationFalseTest() throws Exception {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper,"pricingCampaignRecommendationEnabled",true);
    productLevel3SummaryResponse.getPrices().get(0).setSalePrice(11.0);
    productLevel3SummaryResponse.setPickupPointCode(PICKUP_POINT_CODE);
    LowestPriceRecommendationRequest lowestPriceRecommendationRequest =
        LowestPriceRecommendationRequest.builder().itemSkus(new HashSet<>(Arrays.asList(ITEM_SKU))).build();
    Map<String, LowestPriceResponse> map = new HashMap<>();
    map.put("itemSku-pickupPointCode", new LowestPriceResponse(10.0, false));
    when(promoAnalyticsOutboundService.getLowestPriceRecommendationV2(Mockito.any())).thenReturn(
        new LowestPriceRecommendationResponse(map));
    ArrayList<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    BulkCampaignProductResponse bulkDataResponse = new BulkCampaignProductResponse(productLevel3SummaryResponses);
    List<List<String>> row = this.bulkCampaignProductProcessHelper.getRowData(bulkDataResponse);
    assertNotNull(row);
    Assertions.assertEquals("NO", row.get(0).get(9));
  }

  @Test
  public void getRowDataTest_withRecommedationTrueForLowestPriceResponseFalseTest() throws Exception {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper,"pricingCampaignRecommendationEnabled",true);
    productLevel3SummaryResponse.getPrices().get(0).setSalePrice(11.0);
    productLevel3SummaryResponse.setPickupPointCode(PICKUP_POINT_CODE);
    LowestPriceRecommendationRequest lowestPriceRecommendationRequest =
        LowestPriceRecommendationRequest.builder().itemSkus(new HashSet<>(Arrays.asList(ITEM_SKU))).build();
    Map<String, LowestPriceResponse> map = new HashMap<>();
    map.put("itemSku-pickupPointCode", new LowestPriceResponse(10.0, true));
    when(promoAnalyticsOutboundService.getLowestPriceRecommendationV2(Mockito.any())).thenReturn(null);
    ArrayList<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    BulkCampaignProductResponse bulkDataResponse = new BulkCampaignProductResponse(productLevel3SummaryResponses);
    List<List<String>> row = this.bulkCampaignProductProcessHelper.getRowData(bulkDataResponse);
    assertNotNull(row);
  }

  @Test
  public void getRowDataTest_withRecommedationForLowestPriceIsMaxTest() throws Exception {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper,"pricingCampaignRecommendationEnabled",true);
    productLevel3SummaryResponse.getPrices().get(0).setSalePrice(11.0);
    productLevel3SummaryResponse.setPickupPointCode(PICKUP_POINT_CODE);
    LowestPriceRecommendationRequest lowestPriceRecommendationRequest =
        LowestPriceRecommendationRequest.builder().itemSkus(new HashSet<>(Arrays.asList(ITEM_SKU))).build();
    Map<String, LowestPriceResponse> map = new HashMap<>();
    map.put("itemSku-pickupPointCode", new LowestPriceResponse(Double.MAX_VALUE, true));
    when(promoAnalyticsOutboundService.getLowestPriceRecommendationV2(Mockito.any())).thenReturn(
        new LowestPriceRecommendationResponse(map));
    ArrayList<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    BulkCampaignProductResponse bulkDataResponse = new BulkCampaignProductResponse(productLevel3SummaryResponses);
    List<List<String>> row = this.bulkCampaignProductProcessHelper.getRowData(bulkDataResponse);
    assertNotNull(row);
    Assertions.assertEquals("", row.get(0).get(8));
  }

  @Test
  public void getRowData_mppSwitchOnTest() {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper, "multiPickupPointEnabled",
      true);
    ArrayList<ProductLevel3SummaryResponse> productLevel3SummaryResponses  = new ArrayList<>();
    productLevel3SummaryResponse.setItemName(ITEM_SKU);
    productLevel3SummaryResponse.setProductName(ITEM_SKU);
    productLevel3SummaryResponse.setPickupPointCode(PICKUP_POINT_CODE);
    productLevel3SummaryResponse.setPickupPointName(PICKUP_POINT_NAME);
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    BulkCampaignProductResponse bulkDataResponse = new BulkCampaignProductResponse(productLevel3SummaryResponses);
    List<List<String>> row = this.bulkCampaignProductProcessHelper.getRowData(bulkDataResponse);
    assertNotNull(row);
    Assertions.assertEquals(PICKUP_POINT_CODE, row.get(0).get(1));
    Assertions.assertEquals(String.valueOf(10), row.get(0).get(9));
  }

  @Test
  public void getRowData_mppSwitchOnWithLowestPriceRecommendationTest() {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper, "multiPickupPointEnabled",
        true);
    ArrayList<ProductLevel3SummaryResponse> productLevel3SummaryResponses  = new ArrayList<>();
    productLevel3SummaryResponse.setItemName(ITEM_SKU);
    productLevel3SummaryResponse.setProductName(ITEM_SKU);
    productLevel3SummaryResponse.setPickupPointCode(PICKUP_POINT_CODE);
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    LowestPriceRecommendationRequest lowestPriceRecommendationRequest =
        LowestPriceRecommendationRequest.builder().itemSkus(new HashSet<>(Arrays.asList(ITEM_SKU))).build();
    Map<String, LowestPriceResponse> map = new HashMap<>();
    map.put(ITEM_SKU, new LowestPriceResponse(10.0, true));
    when(promoAnalyticsOutboundService.getLowestPriceRecommendationV2(Mockito.any()))
        .thenReturn(new LowestPriceRecommendationResponse(map));
    BulkCampaignProductResponse bulkDataResponse = new BulkCampaignProductResponse(productLevel3SummaryResponses);
    List<List<String>> row = this.bulkCampaignProductProcessHelper.getRowData(bulkDataResponse);
    assertNotNull(row);
    Assertions.assertEquals(PICKUP_POINT_CODE, row.get(0).get(1));
    Assertions.assertEquals(String.valueOf(10), row.get(0).get(9));
  }

  @Test
  public void getRowData_PricingmppSwitchOnTest() {
    ReflectionTestUtils.setField(bulkCampaignProductProcessHelper, "pricingMultiPickupPointEnabled",
        true);
    ArrayList<ProductLevel3SummaryResponse> productLevel3SummaryResponses  = new ArrayList<>();
    productLevel3SummaryResponse.setItemName(ITEM_SKU);
    productLevel3SummaryResponse.setProductName(ITEM_SKU);
    productLevel3SummaryResponse.setPickupPointCode(PICKUP_POINT_CODE);
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    BulkCampaignProductResponse bulkDataResponse = new BulkCampaignProductResponse(productLevel3SummaryResponses);
    List<List<String>> row = this.bulkCampaignProductProcessHelper.getRowData(bulkDataResponse);
    assertNotNull(row);
    Assertions.assertEquals(PICKUP_POINT_CODE, row.get(0).get(1));
    Assertions.assertEquals(String.valueOf(10), row.get(0).get(9));
  }

  @Test
  public void getDirectoryTest() throws Exception {
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    BulkCampaignProductResponse bulkDataResponse = new BulkCampaignProductResponse(responses);
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setRequestId("test");
    String row = this.bulkCampaignProductProcessHelper.getDirectory(bulkDownloadRequest);
    assertNotNull(row);
  }

  @Test
  public void getFilePathTest() throws Exception {
    String file = this.bulkCampaignProductProcessHelper.getFilePath("FOO", "BAR");
    assertNotNull(file);
  }

  @Test
  public void getEmailParamsTest() throws Exception {
    BulkDownloadRequest request = new BulkDownloadRequest.BulkRequestBuilder().merchant("merchantId")
        .request("requestId").username("username").build();
    Map<String, Object> file = this.bulkCampaignProductProcessHelper.getEmailParams(request, "in");
    assertNotNull(file);
  }

  @Test
  public void getRecordsUpdatedTest() {
    ArrayList<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    productLevel3SummaryResponses.add(new ProductLevel3SummaryResponse());
    BulkCampaignProductResponse bulkCampaignProductResponse =
        new BulkCampaignProductResponse(productLevel3SummaryResponses);
    int countOfUpdatedRecords = bulkCampaignProductProcessHelper.getRecordsUpdated(bulkCampaignProductResponse);
    Assertions.assertEquals(1, countOfUpdatedRecords);
  }

  @Test
  public void generateFileTest() throws Exception{
    this.bulkCampaignProductProcessHelper.generateFile(FILEPATH, CONTENT);
  }

}
