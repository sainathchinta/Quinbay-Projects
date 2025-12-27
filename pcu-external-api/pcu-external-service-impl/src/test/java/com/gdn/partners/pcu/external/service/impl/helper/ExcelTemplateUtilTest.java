package com.gdn.partners.pcu.external.service.impl.helper;

import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.web.model.enums.MerchantType;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandData;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandDataRequest;
import com.gdn.partners.pcu.external.web.model.response.ShippingTypeEligibility;
import com.gdn.x.businesspartner.constant.ProfileFlagNames;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.google.cloud.storage.Blob;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ExcelTemplateUtilTest {
    private static final String PICKUP_POINT_NAME = "PICKUP_POINT_NAME";
    @InjectMocks
    private ExcelTemplateUtil excelTemplateUtil;


    private static final com.gdn.x.businesspartner.dto.PickupPointDTO PICKUP_POINT_DTO = new com.gdn.x.businesspartner.dto.PickupPointDTO();
    private static final List<PickupPointDTO> PICKUP_POINTS_LIST = new ArrayList<>();
    private static final String CODE = "Code";
    private static final String PICKUP_POINT = "pickupPoints";
    XSSFSheet pickupPointsSheet;
    XSSFWorkbook xssfWorkbook = new XSSFWorkbook();
    private static final String REQUEST_ID = "requestId";
    private static final String BRAND_CODE = "brandCode";
    private static final String BRAND_NAME = "brandName";
    private static final String SELLER_CODE = "sellerCode";
    private static final String SELLER_NAME = "sellerName";
    private static final String PICKUP_POINT_NAME_DELIMITER = "||";
    private static final Long BRAND_AUTH_START_DATE = 202314L;
    private static final Long BRAND_AUTH_END_DATE = 202314L;
    private static final String PRODUCT_SKU = "TOQ-60028-00304";
    private static final String DELIVERY_ACTIVE_NEW = "Pengiriman(0 = Off, 1 = On)";
    private static List<BulkBrandData> bulkBrandData = Arrays.asList(
        new BulkBrandData(BRAND_CODE, BRAND_NAME, SELLER_CODE, SELLER_NAME, BRAND_AUTH_START_DATE, BRAND_AUTH_END_DATE));
    private BulkBrandDataRequest bulkBrandDataRequest = new BulkBrandDataRequest();

    @BeforeEach
    public void setUp() {
        PICKUP_POINT_DTO.setCode(CODE);
        PICKUP_POINT_DTO.setName(PICKUP_POINT_NAME);
        PICKUP_POINT_DTO.setFbbActivated(true);
        PICKUP_POINTS_LIST.add(0, PICKUP_POINT_DTO);
        pickupPointsSheet = xssfWorkbook.createSheet("Data");
        bulkBrandDataRequest.setDownloadRequest(bulkBrandData);
    }


    @Test
    public void testSetProductTemplatePickupPoints() {
        Map<String, List<?>> datas = new HashMap<>();
        datas.put(PICKUP_POINT, PICKUP_POINTS_LIST);
        pickupPointsSheet.createRow(1);
        this.excelTemplateUtil.setProductTemplatePickupPoints(datas, pickupPointsSheet, true, false,
            PICKUP_POINT_NAME_DELIMITER);
    }

    @Test
    public void testSetProductTemplatePickupPointsPPNameSwitchTrue() {
        StringBuilder ppNameAndCode = new StringBuilder();
        ppNameAndCode.append(CODE).append(StringUtils.SPACE).append(PICKUP_POINT_NAME_DELIMITER)
            .append(StringUtils.SPACE).append(PICKUP_POINT_NAME);
        Map<String, List<?>> datas = new HashMap<>();
        datas.put(PICKUP_POINT, PICKUP_POINTS_LIST);
        pickupPointsSheet.createRow(1);
        this.excelTemplateUtil.setProductTemplatePickupPoints(datas, pickupPointsSheet, true, true,
            PICKUP_POINT_NAME_DELIMITER);
        Assertions.assertEquals(ppNameAndCode.toString(), pickupPointsSheet.getRow(0).getCell(0).getStringCellValue());
    }
    @Test
    public void testSetProductTemplatePickupPointsFail() {
        Map<String, List<?>> datas = new HashMap<>();
        List<PickupPointDTO> pickupPointDTOS = new ArrayList<>();
        PickupPointDTO pickupPointDTO = new PickupPointDTO();
        pickupPointDTO.setFbbActivated(false);
        pickupPointDTOS.add(pickupPointDTO);
        datas.put(PICKUP_POINT, pickupPointDTOS);
        this.excelTemplateUtil.setProductTemplatePickupPoints(datas, pickupPointsSheet, false, false,
            PICKUP_POINT_NAME_DELIMITER);
    }

    @Test
    public void testSetProductTemplatePickupPointsWhiteListedFbbActiveFalseTest() {
        PICKUP_POINTS_LIST.get(0).setFbbActivated(false);
        Map<String, List<?>> datas = new HashMap<>();
        datas.put(PICKUP_POINT, PICKUP_POINTS_LIST);
        pickupPointsSheet.createRow(1);
        this.excelTemplateUtil.setProductTemplatePickupPoints(datas, pickupPointsSheet, true, false,
            PICKUP_POINT_NAME_DELIMITER);
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOn() {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        ProductLevel3ViewConfigResponse itemViewConfig1 = new ProductLevel3ViewConfigResponse("DEFAULT", true, true);
        ProductLevel3ViewConfigResponse itemViewConfig2 = new ProductLevel3ViewConfigResponse("CNC", true, true);
        productLevel3SummaryResponse.setViewConfigs(Arrays.asList(itemViewConfig1, itemViewConfig2));
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
                .pickupPoints(new ArrayList<>()).build(), false, accessibility,
            Collections.singletonList(productLevel3SummaryResponse), true, true, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOnAccessibilityFalse() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        ProductLevel3ViewConfigResponse itemViewConfig1 = new ProductLevel3ViewConfigResponse("DEFAULT", true, true);
        ProductLevel3ViewConfigResponse itemViewConfig2 = new ProductLevel3ViewConfigResponse("CNC", true, true);
        productLevel3SummaryResponse.setViewConfigs(Arrays.asList(itemViewConfig1, itemViewConfig2));
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        productLevel3SummaryResponse.setPreOrder(true);
        productLevel3SummaryResponse.setPreOrderDate(DateUtils.addDays(new Date(), 2));
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.FALSE);
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
                .pickupPoints(new ArrayList<>()).flags(Map.of(ProfileFlagNames.BLIBLI_OMG, true)).build(),
            false, accessibility, Collections.singletonList(productLevel3SummaryResponse), true,
            true, 1, true);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOnWithoutAccessibility() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        ProductLevel3ViewConfigResponse itemViewConfig1 = new ProductLevel3ViewConfigResponse("DEFAULT", true, true);
        ProductLevel3ViewConfigResponse itemViewConfig2 = new ProductLevel3ViewConfigResponse("CNC", false, false);
        productLevel3SummaryResponse.setViewConfigs(Arrays.asList(itemViewConfig1, itemViewConfig2));
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        productLevel3SummaryResponse.setSynchronizeStock(Boolean.TRUE);
        productLevel3SummaryResponse.setPreOrder(true);
        productLevel3SummaryResponse.setPreOrderDate(null);
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
                .pickupPoints(new ArrayList<>()).flags(Map.of(ProfileFlagNames.BLIBLI_OMG, true)).build(),
            false, accessibility, Collections.singletonList(productLevel3SummaryResponse),
            true, true, 1, true);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOnCNCfalse() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        ProductLevel3ViewConfigResponse itemViewConfig1 = new ProductLevel3ViewConfigResponse("DEFAULT", true, true);
        ProductLevel3ViewConfigResponse itemViewConfig2 = new ProductLevel3ViewConfigResponse("CNC", true, true);
        productLevel3SummaryResponse.setViewConfigs(Arrays.asList(itemViewConfig1, itemViewConfig2));
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        productLevel3SummaryResponse.setPreOrder(false);
        productLevel3SummaryResponse.setPreOrderDate(null);
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(false).build())
                .pickupPoints(new ArrayList<>()).flags(Map.of(ProfileFlagNames.BLIBLI_OMG, true)).build(),
            false, accessibility, Collections.singletonList(productLevel3SummaryResponse),
            true, true, 1, true);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
        AtomicBoolean deliveryHeaderPresent = new AtomicBoolean(false);
        workbook.getSheetAt(0).getRow(0).cellIterator().forEachRemaining(cell -> {
            if (DELIVERY_ACTIVE_NEW.equals(cell.getStringCellValue())) {
                deliveryHeaderPresent.set(true);
            }
        });
        Assertions.assertFalse(deliveryHeaderPresent.get());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOnCNCfalsePureExternal() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        ProductLevel3ViewConfigResponse itemViewConfig1 = new ProductLevel3ViewConfigResponse("DEFAULT", false, true);
        ProductLevel3ViewConfigResponse itemViewConfig2 = new ProductLevel3ViewConfigResponse("CNC", true, true);
        productLevel3SummaryResponse.setViewConfigs(Arrays.asList(itemViewConfig1, itemViewConfig2));
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(false).build())
                .pickupPoints(new ArrayList<>()).flags(Map.of(ProfileFlagNames.BLIBLI_OMG, true)).build(),
            true, accessibility, Collections.singletonList(productLevel3SummaryResponse),
            true, true, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOnCNC_nullStatus() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        ProductLevel3ViewConfigResponse itemViewConfig1 = new ProductLevel3ViewConfigResponse("DEFAULT", null, null);
        ProductLevel3ViewConfigResponse itemViewConfig2 = new ProductLevel3ViewConfigResponse("CNC", true, true);
        productLevel3SummaryResponse.setViewConfigs(Arrays.asList(itemViewConfig1, itemViewConfig2));
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
            .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
            .pickupPoints(new ArrayList<>()).flags(Map.of(ProfileFlagNames.BLIBLI_OMG, false)).build(),
            false, accessibility, Collections.singletonList(productLevel3SummaryResponse),
            true, true, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOnCNC_nullCncStatus() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        ProductLevel3ViewConfigResponse itemViewConfig1 = new ProductLevel3ViewConfigResponse(
          "DEFAULT", true, true);
        ProductLevel3ViewConfigResponse itemViewConfig2 = new ProductLevel3ViewConfigResponse("CNC", null, null);
        productLevel3SummaryResponse.setViewConfigs(Arrays.asList(itemViewConfig1, itemViewConfig2));
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
            .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
            .pickupPoints(new ArrayList<>()).build(), false, accessibility,
          Collections.singletonList(productLevel3SummaryResponse), true, true, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOnCNCTruePureExternal() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        ProductLevel3ViewConfigResponse itemViewConfig1 = new ProductLevel3ViewConfigResponse("DEFAULT", false, true);
        ProductLevel3ViewConfigResponse itemViewConfig2 = new ProductLevel3ViewConfigResponse("CNC", false, true);
        productLevel3SummaryResponse.setViewConfigs(Arrays.asList(itemViewConfig1, itemViewConfig2));
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
                .pickupPoints(new ArrayList<>()).build(), false, accessibility,
            Collections.singletonList(productLevel3SummaryResponse), true, true, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOnCNCTruePureExternal1() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        ProductLevel3ViewConfigResponse itemViewConfig1 = new ProductLevel3ViewConfigResponse("DEFAULT", false, false);
        ProductLevel3ViewConfigResponse itemViewConfig2 = new ProductLevel3ViewConfigResponse("CNC", false, true);
        productLevel3SummaryResponse.setViewConfigs(Arrays.asList(itemViewConfig1, itemViewConfig2));
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
                .pickupPoints(new ArrayList<>()).build(), false, accessibility,
            Collections.singletonList(productLevel3SummaryResponse), true, true, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOnCNCTruePureExternal2() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        ProductLevel3ViewConfigResponse itemViewConfig1 = new ProductLevel3ViewConfigResponse("DEFAULT", false, true);
        ProductLevel3ViewConfigResponse itemViewConfig2 = new ProductLevel3ViewConfigResponse("CNC", false, false);
        productLevel3SummaryResponse.setViewConfigs(Arrays.asList(itemViewConfig1, itemViewConfig2));
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
                .pickupPoints(new ArrayList<>()).build(), false, accessibility,
            Collections.singletonList(productLevel3SummaryResponse), true, true, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOnCNCfalseWithoutAccessibilty() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        ProductLevel3ViewConfigResponse itemViewConfig1 = new ProductLevel3ViewConfigResponse("DEFAULT", true, true);
        ProductLevel3ViewConfigResponse itemViewConfig2 = new ProductLevel3ViewConfigResponse("CNC", true, true);
        productLevel3SummaryResponse.setViewConfigs(Arrays.asList(itemViewConfig1, itemViewConfig2));
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(false).build())
                .pickupPoints(new ArrayList<>()).build(), false, new HashMap<>(),
            Collections.singletonList(productLevel3SummaryResponse), true, true, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOff() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        productLevel3SummaryResponse.setViewConfigs(Collections.singletonList(new ProductLevel3ViewConfigResponse()));
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
                .pickupPoints(new ArrayList<>()).build(), false, accessibility,
            Collections.singletonList(productLevel3SummaryResponse), true, false, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOffWithOutAccessibilty() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        productLevel3SummaryResponse.setViewConfigs(Collections.singletonList(new ProductLevel3ViewConfigResponse()));
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
                .pickupPoints(new ArrayList<>()).build(), false, new HashMap<>(),
            Collections.singletonList(productLevel3SummaryResponse), true, false, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOffPureExternal() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(true);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        productLevel3SummaryResponse.setViewConfigs(Collections.singletonList(new ProductLevel3ViewConfigResponse()));
        Map<String, Boolean> accessibility = new HashMap<>();
        accessibility.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_DISPLAY_BUYABLE, Boolean.TRUE);
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
                .pickupPoints(new ArrayList<>()).build(), true, accessibility,
            Collections.singletonList(productLevel3SummaryResponse), true, false, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }


    @Test
    public void generateWorkbookTemplateBulkUpdateCnc1pSwitchOffCncActiveFalse() throws Exception {
        ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
        productLevel3SummaryResponse.setArchived(false);
        productLevel3SummaryResponse.setCncActive(false);
        productLevel3SummaryResponse.setProductSku(PRODUCT_SKU);
        productLevel3SummaryResponse.setViewConfigs(Collections.singletonList(new ProductLevel3ViewConfigResponse()));
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateBulkUpdate(ProfileResponse.builder()
                .company(CompanyDTO.builder().inventoryFulfillment("BL").cncActivated(true).build())
                .pickupPoints(new ArrayList<>()).build(), false, new HashMap<>(),
            Collections.singletonList(productLevel3SummaryResponse), true, false, 1, false);
        Sheet downloadSelectedSheet = workbook.getSheetAt(0);
        Row row = downloadSelectedSheet.getRow(4);
        Assertions.assertEquals(PRODUCT_SKU,row.getCell(0).getStringCellValue());
    }

    @Test
    public void generateWorkbookTemplateSelectedBrandBulkTest() {
        Workbook workbook = this.excelTemplateUtil.generateWorkbookTemplateSelectedBrandBulk(bulkBrandDataRequest);
        System.out.println(workbook.getSheetAt(0));
        Sheet brandAuthSheet = workbook.getSheetAt(0);
        Row row = brandAuthSheet.getRow(1);
        Assertions.assertEquals("brandName",row.getCell(1).getStringCellValue());
    }

    @Test
    public void getProductTemplateTest() throws Exception {
        Blob blob = mock(Blob.class);
        Map<String, String> files = this.getFiles("products-upload-template");
        byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
        when(blob.getContent()).thenReturn(excelFile);
        XSSFWorkbook sheet = null;
        Assertions.assertThrows(IllegalStateException.class,
            () -> ExcelTemplateUtil.getProductTemplate(new CategoryDetailResponse(),
                ProfileResponse.builder().company(new CompanyDTO()).pickupPoints(new ArrayList<>())
                    .build(), true, new ArrayList<>(), new ArrayList<>(), blob,
                MerchantType.BFB_CNC_SELLER, true, PICKUP_POINT_NAME_DELIMITER,
                new ShippingTypeEligibility(), true, false, false));
    }

    @Test
    void getProductTemplateInstoreTrueTest() throws Exception {
        Blob blob = mock(Blob.class);
        Map<String, String> files = this.getFiles("products-upload-template");
        byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
        when(blob.getContent()).thenReturn(excelFile);
        Assertions.assertThrows(IllegalStateException.class,
            () -> ExcelTemplateUtil.getProductTemplate(new CategoryDetailResponse(),
                ProfileResponse.builder().company(new CompanyDTO()).pickupPoints(new ArrayList<>())
                    .build(), true, new ArrayList<>(), new ArrayList<>(), blob,
                MerchantType.BFB_CNC_SELLER, true, PICKUP_POINT_NAME_DELIMITER,
                new ShippingTypeEligibility(), true, true, false));
    }

    @Test
    public void getProductTemplateInstoreIdTest() throws Exception {
        Blob blob = mock(Blob.class);
        Map<String, String> files = this.getFiles("products-upload-template");
        byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
        when(blob.getContent()).thenReturn(excelFile);
        CompanyDTO companyDTO = new CompanyDTO();
        companyDTO.setOfflineToOnlineFlag(true);
        CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
        CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
        categoryAttributeResponse.setAttribute(AttributeResponse.builder().hideForSeller(false).build());
        categoryDetailResponse.setCategoryAttributes(List.of(categoryAttributeResponse));
        ExcelTemplateUtil.getProductTemplate(categoryDetailResponse,
            ProfileResponse.builder().company(companyDTO).pickupPoints(new ArrayList<>()).build(), false,
            new ArrayList<>(), new ArrayList<>(), blob, MerchantType.BFB_CNC_SELLER, true, PICKUP_POINT_NAME_DELIMITER,
            new ShippingTypeEligibility(), true, true, false);
    }

    @Test
    public void getProductTemplateInstoreIdHideFromSellerTest() throws Exception {
        Blob blob = mock(Blob.class);
        Map<String, String> files = this.getFiles("products-upload-template");
        byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
        when(blob.getContent()).thenReturn(excelFile);
        CompanyDTO companyDTO = new CompanyDTO();
        companyDTO.setOfflineToOnlineFlag(true);
        CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
        CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
        categoryAttributeResponse.setAttribute(AttributeResponse.builder().hideForSeller(true).build());
        categoryDetailResponse.setCategoryAttributes(List.of(categoryAttributeResponse));
        ExcelTemplateUtil.getProductTemplate(categoryDetailResponse,
            ProfileResponse.builder().company(companyDTO).pickupPoints(new ArrayList<>()).build(), false,
            new ArrayList<>(), new ArrayList<>(), blob, MerchantType.BFB_CNC_SELLER, true, PICKUP_POINT_NAME_DELIMITER,
            new ShippingTypeEligibility(), true, true, true);
    }

    @Test
    void getProductTemplateInstoreFalseIdTest() throws Exception {
        Blob blob = mock(Blob.class);
        Map<String, String> files = this.getFiles("products-upload-template");
        byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
        when(blob.getContent()).thenReturn(excelFile);
        CompanyDTO companyDTO = new CompanyDTO();
        companyDTO.setOfflineToOnlineFlag(true);
            Assertions.assertThrows(IllegalStateException.class,
                () -> ExcelTemplateUtil.getProductTemplate(new CategoryDetailResponse(),
                    ProfileResponse.builder().company(companyDTO).pickupPoints(new ArrayList<>())
                        .build(), false, new ArrayList<>(), new ArrayList<>(), blob,
                    MerchantType.BFB_CNC_SELLER, true, PICKUP_POINT_NAME_DELIMITER,
                    new ShippingTypeEligibility(), true, false, false));
    }

    @Test
    void getProductTemplateNonInternationalMerchantTest() throws Exception {
        Blob blob = mock(Blob.class);
        Map<String, String> files = this.getFiles("products-upload-template");
        byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
        when(blob.getContent()).thenReturn(excelFile);
        Assertions.assertThrows(IllegalStateException.class,
            () -> ExcelTemplateUtil.getProductTemplate(new CategoryDetailResponse(),
                ProfileResponse.builder().company(new CompanyDTO()).pickupPoints(new ArrayList<>())
                    .build(), false, new ArrayList<>(), new ArrayList<>(), blob,
                MerchantType.BFB_CNC_SELLER, true, PICKUP_POINT_NAME_DELIMITER,
                new ShippingTypeEligibility(), true, false, false));
    }

    private Map<String, String> getFiles(String fileName) throws Exception {
        Map<String, String> files = new HashMap<String, String>();
        String excelData = new String(Base64.getEncoder().encode(IOUtils.toByteArray(
            Thread.currentThread().getContextClassLoader()
                .getResourceAsStream("Excel-template" + File.separator + fileName + ".xlsx"))), "UTF-8");
        files.put("xls", excelData);
        return files;
    }
}
