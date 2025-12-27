package com.gdn.partners.pcu.external.service.impl.helper;

import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ExcelHeaderNames;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandData;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandDataRequest;
import com.gdn.partners.pcu.external.web.model.response.BulkSelectedDownloadHeaders;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.google.common.collect.ImmutableSet;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class POIUtilTest {
  @InjectMocks
  POIUtil poiUtil;
  private final List<String> HEADER_LIST = new ArrayList<>();
  private static final String CODE = "Code";
  private static final String PICKUP_POINT_SHEETNAME = "Pickup Points";
  private static final String PICKUP_POINT_CODE_HEADER = "Code";
  private static final String PICKUP_POINT_NAME_HEADER = "pickupPointsNames";
  private static PickupPointDTO PICKUP_POINT_DTO = new PickupPointDTO();
  private static List<PickupPointDTO> PICKUP_POINTS = new ArrayList<>();
  public static final ImmutableSet<String> HEADER_LIST_FOR_SELECTED_BULK_DOWNLOAD = ImmutableSet
      .of(ExcelHeaderNames.BRAND_CODE, ExcelHeaderNames.BRAND_NAME, ExcelHeaderNames.SELLER_CODE,
          ExcelHeaderNames.SELLER_NAME, ExcelHeaderNames.AUTH_START_DATE, ExcelHeaderNames.AUTH_END_DATE);
  private static final String REQUEST_ID = "requestId";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String SELLER_CODE = "sellerCode";
  private static final String SELLER_NAME = "sellerName";
  private static final Long BRAND_AUTH_START_DATE = 202314L;
  private static final Long BRAND_AUTH_END_DATE = 202314L;
  private static List<BulkBrandData> bulkBrandData = Arrays.asList(
      new BulkBrandData(BRAND_CODE, BRAND_NAME, SELLER_CODE, SELLER_NAME, BRAND_AUTH_START_DATE, BRAND_AUTH_END_DATE));
  private BulkBrandDataRequest bulkBrandDataRequest = new BulkBrandDataRequest();
  private BulkSelectedDownloadHeaders bulkSelectedDownloadHeaders;
  private HashMap<Integer, List<String>> products;

  @BeforeEach
  public void setUp() {
    PICKUP_POINT_DTO.setCode(CODE);
    PICKUP_POINT_DTO.setFbbActivated(true);
    HEADER_LIST.add("Header 1");
    HEADER_LIST.add("Header 2");
    HEADER_LIST.add("Header 3");
    PICKUP_POINTS.add(0, PICKUP_POINT_DTO);
    bulkBrandDataRequest.setDownloadRequest(bulkBrandData);
    bulkSelectedDownloadHeaders = new BulkSelectedDownloadHeaders();
    bulkSelectedDownloadHeaders.setPrimaryHeaders(List.of(ExcelHeaderNames.PRODUCT_NAME));
    bulkSelectedDownloadHeaders.setSecondaryHeaders(List.of(ExcelHeaderNames.PRODUCT_NAME));
    bulkSelectedDownloadHeaders.setTertiaryHeaders(List.of(ExcelHeaderNames.PRODUCT_NAME));
    bulkSelectedDownloadHeaders.setQuaternaryHeaders(List.of(ExcelHeaderNames.PRODUCT_NAME));
    products = new HashMap<>();
    products.put(1, List.of(PICKUP_POINT_CODE_HEADER));
  }

  @Test
  public void testGenerateWorkbookForMPPTemplateForFBBTrue() {
    SXSSFWorkbook workbook = poiUtil.generateWorkbookForMPPTemplate(HEADER_LIST, PICKUP_POINTS, PICKUP_POINT_SHEETNAME,
        PICKUP_POINT_CODE_HEADER, PICKUP_POINT_NAME_HEADER, true);
    Sheet pickupPointSheet = workbook.getSheetAt(1);
    Row firstDataRow = pickupPointSheet.getRow(1);
    Assertions.assertEquals(Constants.FBB, firstDataRow.getCell(2).getStringCellValue());
  }

  @Test
  public void testGenerateWorkbookForMPPTemplateForFBBFalse() {
    PICKUP_POINT_DTO.setFbbActivated(false);
    SXSSFWorkbook workbook = poiUtil.generateWorkbookForMPPTemplate(HEADER_LIST, PICKUP_POINTS, PICKUP_POINT_SHEETNAME,
        PICKUP_POINT_CODE_HEADER, PICKUP_POINT_NAME_HEADER, false);
    Sheet pickupPointSheet = workbook.getSheetAt(1);
    Row firstDataRow = pickupPointSheet.getRow(1);
    Assertions.assertEquals("Code", firstDataRow.getCell(0).getStringCellValue());
  }

  @Test
  public void testGenerateWorkbookForMPPTemplateForWhiteListedFBBFalse() {
    PICKUP_POINTS.get(0).setFbbActivated(false);
    SXSSFWorkbook workbook = poiUtil
      .generateWorkbookForMPPTemplate(HEADER_LIST, PICKUP_POINTS, PICKUP_POINT_SHEETNAME,
        PICKUP_POINT_CODE_HEADER, PICKUP_POINT_NAME_HEADER, true);
    Sheet pickupPointSheet = workbook.getSheetAt(1);
    Row firstDataRow = pickupPointSheet.getRow(1);
    Assertions.assertEquals("Code", firstDataRow.getCell(0).getStringCellValue());
  }

  @Test
  public void generateWorkbookForBrandAuthTest(){
    SXSSFWorkbook workbook = poiUtil.generateWorkbookForBrandAuth(HEADER_LIST_FOR_SELECTED_BULK_DOWNLOAD,bulkBrandDataRequest);
    Sheet brandSheet = workbook.getSheetAt(0);
    Row firstDataRow = brandSheet.getRow(1);
    Assertions.assertEquals("brandCode",firstDataRow.getCell(0).getStringCellValue());

  }

  @Test
  public void generateXLFileTest() {
    SXSSFWorkbook workbook =
        POIUtil.generateXLFile(new HashMap<>(), List.of(new PickupPointDTO()), false, bulkSelectedDownloadHeaders, 1);
    bulkSelectedDownloadHeaders.setSecondaryHeaders(List.of(Constants.MANDATORY_COLUMN_ID));
    bulkSelectedDownloadHeaders.setTertiaryHeaders(List.of(Constants.EDITABLE_COLUMN_ID));
    Sheet data = workbook.getSheetAt(0);
    Row firstDataRow = data.getRow(0);
    Assertions.assertEquals(ExcelHeaderNames.PRODUCT_NAME, firstDataRow.getCell(0).getStringCellValue());
    workbook = POIUtil.generateXLFile(products, List.of(new PickupPointDTO()), false, bulkSelectedDownloadHeaders, 1);
    data = workbook.getSheetAt(0);
    firstDataRow = data.getRow(0);
    Assertions.assertEquals(ExcelHeaderNames.PRODUCT_NAME, firstDataRow.getCell(0).getStringCellValue());
  }
}
