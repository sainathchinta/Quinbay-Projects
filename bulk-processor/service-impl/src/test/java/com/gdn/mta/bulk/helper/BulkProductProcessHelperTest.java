package com.gdn.mta.bulk.helper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.models.download.responsedata.PickupPointModel;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.GenericBulkParameters;
import com.gdn.partners.bulk.util.Constant;

import com.gdn.x.businesspartner.constant.ProfileFlagNames;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFPalette;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.hssf.util.HSSFColor;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.ProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * Created by keshashah on 17/11/16.
 */
public class BulkProductProcessHelperTest {

  @InjectMocks
  private BulkProductProcessHelper bulkProductProcessHelper;

  @Mock
  Constant constant;

  @Mock
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private HSSFColor mockColor;

  private static final String HEADER_1 = "header1";
  private static final String HEADER_2 = "header2";
  private static final String INFO_PRODUCT = "Info produk";
  private static final String NOTES = "Baris : 2. Variasi Invalid attribute value. ";
  private static final String BUSINESS_PARTNER = "CODE";
  private static final String PICKUP_POINT_NAME = "pickupPoint Name";
  private static final String PICKUP_POINT_CODE = "pickupPoint Code";
  private List<List<String>> rawExcelHeaders1;
  private List<List<String>> invalidRows1;
  private BulkProductResponse bulkProductResponse;
  private HSSFWorkbook mockWorkbook;
  private HSSFPalette mockPalette;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    mockWorkbook = new HSSFWorkbook();
    mockPalette = mockWorkbook.getCustomPalette();
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_PRICE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_STOCK, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_PRODUCT_TYPE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_PICKUP_POINT, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_O2O, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    bulkProductResponse =
      new BulkProductResponse(invalidRows1, false, privilegedMap, Collections.emptyList(),
        Collections.emptyMap(),false);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.bulkDownloadServiceBeanUtil);
    Mockito.verifyNoMoreInteractions(businessPartnerRepository, objectMapper);
  }

  @Test
  public void getHeaderList_emptyPriviliagedMap_NoEntryInHeaders() throws Exception {
    BulkProductResponse productResponse =
        new BulkProductResponse(new ArrayList<List<String>>(), false,
            new HashMap<String, Boolean>(), new HashMap<String, String>());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(false).merchantType(Constant.CC_MERCHANT).build());
    when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, productResponse.getBusinessPartnerCode()))
        .thenReturn(profileResponse);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    assertEquals(4, headerList.size());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, productResponse.getBusinessPartnerCode());
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }

  @Test
  public void getHeaderList_isPrivilegedToReadPricePriviliagedMap() throws Exception {
    ReflectionTestUtils.setField(bulkProductProcessHelper, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkProductProcessHelper, "preOrderQuotaFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(false).merchantType(Constant.CC_MERCHANT).build());
    profileResponse.setMultiDefaultAddressFlag(true);
    profileResponse.setFlags(Map.of(ProfileFlagNames.BLIBLI_OMG, Boolean.TRUE));
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER))
      .thenReturn(profileResponse);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("isPrivilegedToReadPrice", true);
    privilegedMap.put("isPrivilegedToReadAvailableStock", true);
    privilegedMap.put("isPrivilegedToReadProductType", true);
    privilegedMap.put("isPrivilegedToReadPickupPoint", true);
    privilegedMap.put("isPrivilegedToReadDisplayBuyable", true);
    privilegedMap.put("isPrivilegedToReadO2O", true);
    privilegedMap.put("isOnlyExternalUser", false);
    BulkProductResponse productResponse =
        new BulkProductResponse(new ArrayList<List<String>>(), true,
            privilegedMap, new HashMap<String, String>());
    productResponse.setBusinessPartnerCode(BUSINESS_PARTNER);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER);
    assertEquals(4, headerList.size());
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }

  @Test
  public void getHeaderList_isPrivilegedToReadPriceAndB2bPriviliagedMap() throws Exception {
    ReflectionTestUtils.setField(bulkProductProcessHelper, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkProductProcessHelper, "preOrderQuotaFeatureSwitch", false);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(false).merchantType(Constant.CC_MERCHANT).build());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(Constant.B2B_SELLER_CHANNEL));
    profileResponse.setMultiDefaultAddressFlag(true);
    profileResponse.setFlags(Map.of(ProfileFlagNames.BLIBLI_OMG, Boolean.TRUE));
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER))
        .thenReturn(profileResponse);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("isPrivilegedToReadPrice", true);
    privilegedMap.put("isPrivilegedToReadAvailableStock", true);
    privilegedMap.put("isPrivilegedToReadProductType", true);
    privilegedMap.put("isPrivilegedToReadPickupPoint", true);
    privilegedMap.put("isPrivilegedToReadDisplayBuyable", true);
    privilegedMap.put("isPrivilegedToReadO2O", true);
    privilegedMap.put("isOnlyExternalUser", false);
    BulkProductResponse productResponse =
        new BulkProductResponse(new ArrayList<List<String>>(), true,
            privilegedMap, new HashMap<String, String>());
    productResponse.setBusinessPartnerCode(BUSINESS_PARTNER);
    String HEADERS =
        "[\"Blibli Product SKU\",\"Parent Product name\",\"Blibli SKU\",\"Toko/Gudang\",\"Nama "
            + "Produk\",\"SKU Code\",\"Seller SKU\",\"Harga (Rp)\",\"Harga Penjualan (Rp)\","
            + "\"Stok\",\"Tipe Penanganan\",\"Status/pengiriman(0 = Offline, 1 = Online, 2 = Akan"
            + " datang, 3 = Dibeli via link)\",\"Instore\",\"Bfb Harga dasar\",\"Bfb Diatur(1 = "
            + "Yes, 0=No)\",\"Bfb Status(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via"
            + " link)\"]";
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(HEADERS);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    List<String> primaryHeaders =
        new ObjectMapper().readValue(headerList.get(0), new TypeReference<List<String>>() {});
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER);
    assertEquals(16, primaryHeaders.size());
    assertTrue(primaryHeaders.contains(BulkParameters.BFB_BASE_PRICE));
    assertTrue(primaryHeaders.contains(BulkParameters.BFB_MANAGED));
    assertTrue(primaryHeaders.contains(BulkParameters.AMPHI_BFB_STATUS));
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }


  @Test
  public void getHeaderList_isPrivilegedToReadPriceAndB2bExternalPriviliagedMap() throws Exception {
    ReflectionTestUtils.setField(bulkProductProcessHelper, "multiPickupPointEnabled", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(false).merchantType(Constant.CC_MERCHANT).build());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(Constant.B2B_SELLER_CHANNEL));
    profileResponse.setMultiDefaultAddressFlag(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER))
        .thenReturn(profileResponse);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("isPrivilegedToReadPrice", true);
    privilegedMap.put("isPrivilegedToReadAvailableStock", true);
    privilegedMap.put("isPrivilegedToReadProductType", true);
    privilegedMap.put("isPrivilegedToReadPickupPoint", true);
    privilegedMap.put("isPrivilegedToReadDisplayBuyable", true);
    privilegedMap.put("isPrivilegedToReadO2O", true);
    privilegedMap.put("isOnlyExternalUser", true);
    BulkProductResponse productResponse =
        new BulkProductResponse(new ArrayList<List<String>>(), true,
            privilegedMap, new HashMap<String, String>());
    productResponse.setBusinessPartnerCode(BUSINESS_PARTNER);
    String HEADERS =
        "[\"Bfb Harga dasar\",\"Bfb Diatur(1 = Yes, 0=No)\",\"Bfb Status(0 = Offline, 1 = Online)"
            + "\"]";
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(HEADERS);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER);
    assertEquals(4, headerList.size());
    List<String> primaryHeader =
        new ObjectMapper().readValue(headerList.get(0), new TypeReference<List<String>>() {});
    assertTrue(primaryHeader.contains(BulkParameters.BFB_BASE_PRICE));
    assertTrue(primaryHeader.contains(BulkParameters.BFB_MANAGED));
    assertTrue(primaryHeader.contains(BulkParameters.EXTERNAL_BFB_STATUS));
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }

  @Test
  public void getHeaderList_pureExternalWarehouseO2OMerchant() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER))
      .thenReturn(profileResponse);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("isPrivilegedToReadPrice", true);
    privilegedMap.put("isPrivilegedToReadAvailableStock", true);
    privilegedMap.put("isPrivilegedToReadProductType", true);
    privilegedMap.put("isPrivilegedToReadPickupPoint", true);
    privilegedMap.put("isPrivilegedToReadDisplayBuyable", true);
    privilegedMap.put("isPrivilegedToReadO2O", true);
    privilegedMap.put("isOnlyExternalUser", true);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, true);
    BulkProductResponse productResponse =
        new BulkProductResponse(new ArrayList<List<String>>(), true,
            privilegedMap, new HashMap<String, String>());
    productResponse.setBusinessPartnerCode(BUSINESS_PARTNER);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER);
    assertEquals(4, headerList.size());
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }

  @Test
  public void getHeaderList_warehouseStockPrivileged() throws Exception {
    Map<String, Boolean> privilegedMap = new HashMap<String, Boolean>();
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    BulkProductResponse productResponse =
        new BulkProductResponse(new ArrayList<List<String>>(), false,
            privilegedMap, new HashMap<String, String>());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).merchantType(Constant.CC_MERCHANT).build());
    when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, productResponse.getBusinessPartnerCode()))
        .thenReturn(profileResponse);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, productResponse.getBusinessPartnerCode());
    assertEquals(4, headerList.size());
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }

  @Test
  public void getHeaderListWarehouseStockPrivilegedMpp() throws Exception {
    ReflectionTestUtils.setField(bulkProductProcessHelper, "multiPickupPointEnabled", true);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    BulkProductResponse productResponse =
        new BulkProductResponse(new ArrayList<>(), false, privilegedMap, new HashMap<>());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).merchantType(Constant.CC_MERCHANT).build());
    when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, productResponse.getBusinessPartnerCode()))
        .thenReturn(profileResponse);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, productResponse.getBusinessPartnerCode());
    assertEquals(4, headerList.size());
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }

  @Test
  public void modifyWorkbook() throws Exception {
    this.bulkProductProcessHelper.modifyWorkbook(new XSSFWorkbook(), bulkProductResponse);
    Mockito.verify(this.bulkDownloadServiceBeanUtil)
      .generateValidationForWorkbook(Mockito.any(XSSFWorkbook.class), eq(0), eq(3),
        eq(true), eq(BulkParameters.PICKUP_POINT_SHEET));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProductResponse.getBusinessPartnerCode());
  }

  @Test
  public void modifyWorkbookwithPickupPointfbbTrue() throws Exception {
    List<PickupPointModel> pickupPointModelList = new ArrayList<>();
    PickupPointModel pickupPointModel = new PickupPointModel(PICKUP_POINT_NAME,PICKUP_POINT_CODE,true);
    pickupPointModelList.add(pickupPointModel);
    bulkProductResponse.setPickupPoint(pickupPointModelList);
    this.bulkProductProcessHelper.modifyWorkbook(new XSSFWorkbook(), bulkProductResponse);
    Mockito.verify(this.bulkDownloadServiceBeanUtil)
        .generateValidationForWorkbook(Mockito.any(XSSFWorkbook.class), eq(1), eq(3),
            eq(true), eq(BulkParameters.PICKUP_POINT_SHEET));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProductResponse.getBusinessPartnerCode());
  }

  @Test
  public void modifyWorkbookwithPickupPointfbbFalse() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProductResponse.getBusinessPartnerCode()))
        .thenReturn(profileResponse);
    List<PickupPointModel> pickupPointModelList = new ArrayList<>();
    PickupPointModel pickupPointModel = new PickupPointModel(PICKUP_POINT_NAME,PICKUP_POINT_CODE,false);
    pickupPointModelList.add(pickupPointModel);
    bulkProductResponse.setPickupPoint(pickupPointModelList);
    this.bulkProductProcessHelper.modifyWorkbook(new XSSFWorkbook(), bulkProductResponse);
    Mockito.verify(this.bulkDownloadServiceBeanUtil)
        .generateValidationForWorkbook(Mockito.any(XSSFWorkbook.class), eq(1), eq(3),
            eq(true), eq(BulkParameters.PICKUP_POINT_SHEET));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProductResponse.getBusinessPartnerCode());
  }

  @Test
  public void modifyWorkbookwithPickupPointWhitelistedFalsefbbFalse() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(true);
    when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProductResponse.getBusinessPartnerCode()))
        .thenReturn(profileResponse);
    List<PickupPointModel> pickupPointModelList = new ArrayList<>();
    PickupPointModel pickupPointModel = new PickupPointModel(PICKUP_POINT_NAME,PICKUP_POINT_CODE,false);
    pickupPointModelList.add(pickupPointModel);
    bulkProductResponse.setPickupPoint(pickupPointModelList);
    this.bulkProductProcessHelper.modifyWorkbook(new XSSFWorkbook(), bulkProductResponse);
    Mockito.verify(this.bulkDownloadServiceBeanUtil)
      .generateValidationForWorkbook(Mockito.any(XSSFWorkbook.class), eq(1), eq(3),
        eq(true), eq(BulkParameters.PICKUP_POINT_SHEET));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProductResponse.getBusinessPartnerCode());
  }

  @Test
  public void modifyWorkbookwithPickupPointWhitelistedFalsefbbTrue() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(true);
    profileResponse.setFbbActivated(true);
    when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProductResponse.getBusinessPartnerCode()))
        .thenReturn(profileResponse);
    List<PickupPointModel> pickupPointModelList = new ArrayList<>();
    PickupPointModel pickupPointModel = new PickupPointModel(PICKUP_POINT_NAME,PICKUP_POINT_CODE,true);
    pickupPointModelList.add(pickupPointModel);
    bulkProductResponse.setPickupPoint(pickupPointModelList);
    this.bulkProductProcessHelper.modifyWorkbook(new XSSFWorkbook(), bulkProductResponse);
    Mockito.verify(this.bulkDownloadServiceBeanUtil)
        .generateValidationForWorkbook(Mockito.any(XSSFWorkbook.class), eq(1), eq(3),
            eq(true), eq(BulkParameters.PICKUP_POINT_SHEET));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProductResponse.getBusinessPartnerCode());
  }

  @Test
  public void getRowData() throws Exception {
    List<List<String>> testData = new ArrayList<>();
    testData.add(new ArrayList<String>());
    BulkProductResponse productResponse =
        new BulkProductResponse(testData, false, new HashMap<String, Boolean>(), new HashMap<String, String>());
    List<List<String>> rowData = bulkProductProcessHelper.getRowData(productResponse);
    assertEquals(testData.size(), rowData.size());
  }

  @Test
  public void getDirectory() throws Exception {
    ProductDownloadRequest.ProductBuilder builder = new ProductDownloadRequest.ProductBuilder();
    builder.request("123");
    ProductDownloadRequest request = builder.build();
    String directory = bulkProductProcessHelper.getDirectory(request);
    assertTrue(directory.endsWith(request.getRequestId()));
    assertTrue(directory.startsWith(ProcessorUtils.BULK_DOWNLOAD_DIR));
  }


  @Test
  public void getEmailParams() throws Exception {
    ProductDownloadRequest.ProductBuilder builder = new ProductDownloadRequest.ProductBuilder();
    builder.request("123");
    builder.username("kesha@gmail.com");
    builder.merchant("M-123");
    ProductDownloadRequest request = builder.build();
    Map<String, Object> emailParams = bulkProductProcessHelper.getEmailParams(request, request.getLanguage());
    assertEquals("kesha", emailParams.get("name"));
    assertEquals(request.getMerchantId(), emailParams.get("businessPartnerCode"));
    assertEquals(request.getRequestId(), emailParams.get("reqId"));
    assertTrue(emailParams.containsKey(EmailConstants.TEMPLATE_ID_PARAM));
    assertTrue(emailParams.containsKey(EmailConstants.MAIL_SENDER_PARAM));
    assertTrue(emailParams.containsKey(EmailConstants.MAIL_SUBJECT_PARAM));
  }

  @Test
  public void testGetFailedProductDirectory() {
    String path = bulkProductProcessHelper.getFailedProductDirectory("code");
    assertTrue(path.contains("/code"));
  }

  @Test
  public void getRecordsUpdated() throws Exception {
    assertNotNull(constant.BLIBLI_SKU_HEADER);
    assertNotNull(constant.EXCEL_FILE_MUST_NOT_BE_BLANK);
    assertNotNull(constant.EXCEL_FILE_NAME);
    assertNotNull(constant.EXCEL_FILE_NAME_MUST_NOT_BE_BLANK);
    assertNotNull(constant.FINISH_UPLOADING);
    assertNotNull(constant.ITEM_SKU_PATTERN);
    assertNotNull(constant.PROCESSOR_SERVICE);
    assertNotNull(constant.PRODUCT_TYPE_REGULAR);
    assertNotNull(constant.PRODUCT_TYPE_REGULAR_WEIGHT_THRESHOLD);
    assertNotNull(constant.SHEET);
    assertNotNull(constant.SHEET_2);
    assertNotNull(constant.SHEET_1);
    assertNotNull(constant.SHEET_3);
    assertNotNull(constant.SHEET_4);
  }

  @Test
  public void generateDataSheetWithHSSFWorkBookTest() {
    List<List<String>> headers = new ArrayList<>();
    List<String> header = Arrays.asList(HEADER_1, HEADER_2, StringUtils.EMPTY);
    headers.add(header);
    headers.add(header);
    headers.add(header);
    headers.add(header);
    List<List<String>> rows = new ArrayList<>();
    Workbook response = bulkProductProcessHelper.generateDataSheetWithHSSFWorkBook(headers, rows,
        BulkParameters.DATA_SHEET, true);
    assertNotNull(response);
  }

  @Test
  public void generateDataSheetWithHSSFWorkBookMergeableTrueTest() {
    List<List<String>> headers = new ArrayList<>();
    List<String> header = Arrays.asList(HEADER_1, HEADER_2, StringUtils.EMPTY);
    headers.add(header);
    headers.add(header);
    headers.add(header);
    headers.add(header);
    List<List<String>> rows = new ArrayList<>();
    Workbook response = bulkProductProcessHelper.generateDataSheetWithHSSFWorkBook(headers, rows,
        BulkParameters.DATA_SHEET, true);
    assertNotNull(response);
  }

  @Test
  public void generateDataSheetWithHSSFWorkBookMergeableFalseTest() {
    List<List<String>> headers = new ArrayList<>();
    List<String> header = Arrays.asList(HEADER_1, HEADER_2);
    headers.add(header);
    headers.add(header);
    headers.add(header);
    headers.add(header);
    List<List<String>> rows = new ArrayList<>();
    Workbook response = bulkProductProcessHelper.generateDataSheetWithHSSFWorkBook(headers, rows,
        BulkParameters.DATA_SHEET, false);
    assertNotNull(response);
  }

  @Test
  public void generateDataSheetWithSXSSFWorkbookTest() throws IOException {
    String HEADERS = "[\"Bfb Status(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via link)\"]";

    List<String> headers = Arrays.asList(HEADERS, HEADERS, HEADERS, HEADERS);
    List<List<String>> rows = new ArrayList<>();
    List<String> rowData = Arrays.asList(HEADER_1, HEADER_2);
    rows.add(rowData);
    Mockito.when(objectMapper.readValue(eq(HEADERS), Mockito.any(TypeReference.class))).thenReturn(List.of(HEADERS));
    Workbook response = bulkProductProcessHelper.generateDataSheet(headers, rows, 1);
    Mockito.verify(objectMapper, times(4)).readValue(eq(HEADERS), Mockito.any(TypeReference.class));
    assertNotNull(response);
  }

  @Test
  @Disabled
  public void generateDataSheetWithSXSSFWorkbook_emptyDataTest() throws IOException {
    List<String> headers = Collections.EMPTY_LIST;
    List<List<String>> rows = Collections.EMPTY_LIST;
    Workbook response = bulkProductProcessHelper.generateDataSheet(headers, rows, 1);
    assertNotNull(response);
  }

  @Test
  @Disabled
  public void generateDataSheetWithSXSSFWorkbook_emptyHeadersTest() throws IOException {
    List<String> headers = Collections.EMPTY_LIST;
    List<List<String>> rows = Collections.EMPTY_LIST;
    Workbook response = bulkProductProcessHelper.generateDataSheet(headers, rows, 1);
    assertNotNull(response);
  }


  @Test
  public void generateDataSheetWithSXSSFWorkbook_withProductSkuTest() throws IOException {
    String HEADERS = "[\"Blibli Product SKU\"]";
    List<String> headers = Arrays.asList(HEADERS, HEADERS, HEADERS, HEADERS);
    List<List<String>> rows = new ArrayList<>();
    List<String> rowData = Arrays.asList(BulkParameters.BLIBLI_PRODUCT_SKU);
    rows.add(rowData);
    Mockito.when(objectMapper.readValue(eq(HEADERS), Mockito.any(TypeReference.class))).thenReturn(List.of(HEADERS));
    Workbook response = bulkProductProcessHelper.generateDataSheet(headers, rows, 1);
    Mockito.verify(objectMapper, times(4)).readValue(eq(HEADERS), Mockito.any(TypeReference.class));
    assertNotNull(response);
  }

  @Test
  public void generateDataSheetWithSXSSFWorkbook_withProductNameTest() throws IOException {
    String HEADERS = "[\"Parent Product name\"]";
    List<String> headers = Arrays.asList(HEADERS, HEADERS, HEADERS, HEADERS);
    List<List<String>> rows = new ArrayList<>();
    List<String> rowData = Arrays.asList(BulkParameters.PARENT_PRODUCT_NAME);
    rows.add(rowData);
    Mockito.when(objectMapper.readValue(eq(HEADERS), Mockito.any(TypeReference.class))).thenReturn(List.of(HEADERS));
    Workbook response = bulkProductProcessHelper.generateDataSheet(headers, rows, 1);
    Mockito.verify(objectMapper, times(4)).readValue(eq(HEADERS), Mockito.any(TypeReference.class));
    assertNotNull(response);
  }

  @Test
  public void generateDataSheetWithSXSSFWorkbook_withViewConfigExternalTest() throws IOException {
    String HEADERS = "[\"Status/pengiriman(0 = Offline, 1 = Online)\"]";
    List<String> headers = Arrays.asList(HEADERS, HEADERS, HEADERS, HEADERS);
    List<List<String>> rows = new ArrayList<>();
    List<String> rowData = Arrays.asList(BulkParameters.SKU_STATUS_ONLINE);
    rows.add(rowData);
    Mockito.when(objectMapper.readValue(eq(HEADERS), Mockito.any(TypeReference.class))).thenReturn(List.of(HEADERS));
    Workbook response = bulkProductProcessHelper.generateDataSheet(headers, rows, 1);
    Mockito.verify(objectMapper, times(4)).readValue(eq(HEADERS), Mockito.any(TypeReference.class));
    assertNotNull(response);
  }

  @Test
  public void generateDataSheetWithSXSSFWorkbook_withViewConfigAmphiTest() throws IOException {
    Mockito.when(objectMapper.writeValueAsString(Mockito.any()))
        .thenReturn(BulkParameters.AMPHI_SKU_STATUS);
    String HEADERS =
        "[\"Seller SKU\", "
            + "\"Status/pengiriman(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via link)\", "
            + "\"Bisa diedit\", \"(Wajib)\"]";
    List<String> headers = new ArrayList<>();
    headers.add(HEADERS);
    headers.add(HEADERS);
    headers.add(HEADERS);
    headers.add(HEADERS);
    List<List<String>> rows = new ArrayList<>();
    List<String> rowData = Arrays.asList(BulkParameters.SKU_STATUS_B2B);
    rows.add(rowData);
    Mockito.when(objectMapper.readValue(eq(HEADERS), Mockito.any(TypeReference.class))).thenReturn(List.of(HEADERS));
    Workbook response = bulkProductProcessHelper.generateDataSheet(headers, rows, 1);
    Mockito.verify(objectMapper, times(4)).readValue(eq(HEADERS), Mockito.any(TypeReference.class));
    assertNotNull(response);
  }

  @Test
  public void generateGenericDataSheetWithHSSFWorkBookTest() {
    setHeader();
    Workbook response = bulkProductProcessHelper
        .generateDataSheetWithHSSFWorkBook(rawExcelHeaders1, invalidRows1, GenericBulkParameters.USER_INPUT_DATA_SHEET,
            false);
    assertNotNull(response);
    assertEquals(INFO_PRODUCT, response.getSheetAt(0).getRow(0).getCell(1).getStringCellValue());
    assertEquals(NOTES, response.getSheetAt(0).getRow(5).getCell(42).getStringCellValue());
  }

  @Test
  void testGetClosestHSSFColorIndex_WhenColorExists() {
    byte[] rgb = {(byte) 255, (byte) 0, (byte) 0};
    HSSFColor color = mockPalette.findColor(rgb[0], rgb[1], rgb[2]);
    short result = BulkProcessHelper.getClosestHSSFColorIndex(rgb, mockWorkbook);
    assertNotNull(color);
    assertEquals(color.getIndex(), result);
  }

  @Test
  void testGetClosestHSSFColorIndex_WhenColorNotFound() {
    byte[] rgb = {(byte) 123, (byte) 123, (byte) 123};
    short result = BulkProcessHelper.getClosestHSSFColorIndex(rgb, mockWorkbook);
    assertEquals(HSSFColor.HSSFColorPredefined.LIGHT_ORANGE.getIndex(), result);
  }

  @Test
  public void generateGenericDataSheetWithHSSFWorkBookSellerSkuTest() {
    setHeader();
    rawExcelHeaders1.get(3).set(2, "Seller SKU");
    Workbook response = bulkProductProcessHelper
        .generateDataSheetWithHSSFWorkBook(rawExcelHeaders1, invalidRows1, GenericBulkParameters.USER_INPUT_DATA_SHEET,
            false);
    assertNotNull(response);
    assertEquals(INFO_PRODUCT, response.getSheetAt(0).getRow(0).getCell(1).getStringCellValue());
    assertEquals(NOTES, response.getSheetAt(0).getRow(5).getCell(42).getStringCellValue());
  }

  @Test
  public void getHeaderList_CncMerchant() throws Exception {
    ReflectionTestUtils.setField(bulkProductProcessHelper, "multiPickupPointEnabled", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).merchantType(Constant.MERCHANT_TYPE_CM).build());
    profileResponse.setMultiDefaultAddressFlag(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER))
      .thenReturn(profileResponse);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("isPrivilegedToReadPrice", true);
    privilegedMap.put("isPrivilegedToReadAvailableStock", true);
    privilegedMap.put("isPrivilegedToReadProductType", true);
    privilegedMap.put("isPrivilegedToReadPickupPoint", true);
    privilegedMap.put("isPrivilegedToReadDisplayBuyable", true);
    privilegedMap.put("isPrivilegedToReadO2O", true);
    privilegedMap.put("isOnlyExternalUser", true);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, true);
    BulkProductResponse productResponse =
      new BulkProductResponse(new ArrayList<List<String>>(), true,
        privilegedMap, new HashMap<String, String>());
    productResponse.setBusinessPartnerCode(BUSINESS_PARTNER);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID,
      BUSINESS_PARTNER);
    assertEquals(4, headerList.size());
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }

  @Test
  public void getHeaderList_Cnc1P() throws Exception {
    ReflectionTestUtils.setField(bulkProductProcessHelper, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkProductProcessHelper, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).merchantType(Constant.MERCHANT_TYPE_CM).build());
    profileResponse.setMultiDefaultAddressFlag(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER))
        .thenReturn(profileResponse);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("isPrivilegedToReadPrice", true);
    privilegedMap.put("isPrivilegedToReadAvailableStock", true);
    privilegedMap.put("isPrivilegedToReadProductType", true);
    privilegedMap.put("isPrivilegedToReadPickupPoint", true);
    privilegedMap.put("isPrivilegedToReadDisplayBuyable", true);
    privilegedMap.put("isPrivilegedToReadO2O", true);
    privilegedMap.put("isOnlyExternalUser", true);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, true);
    BulkProductResponse productResponse =
        new BulkProductResponse(new ArrayList<List<String>>(), true,
            privilegedMap, new HashMap<String, String>());
    String HEADERS = "[\"Click & Collect(0= Off, 1 = On)\",\"Pengiriman(0 = Off, 1 = On)\"]";
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(HEADERS);
    productResponse.setBusinessPartnerCode(BUSINESS_PARTNER);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER);
    assertEquals(4, headerList.size());
    List<String> primaryHeaders =
        new ObjectMapper().readValue(headerList.get(0), new TypeReference<List<String>>() {});
    assertTrue(primaryHeaders.contains(BulkParameters.CNC_STATUS_HEADER));
    ReflectionTestUtils.setField(bulkProductProcessHelper, "cncForWarehouseFeatureSwitch", false);
    ReflectionTestUtils.setField(bulkProductProcessHelper, "cncForWarehouseFeatureSwitch", false);
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }

  @Test
  public void getHeaderList_Cnc1PNonCnc() throws Exception {
    ReflectionTestUtils.setField(bulkProductProcessHelper, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkProductProcessHelper, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(false).merchantType(Constant.MERCHANT_TYPE_CM).build());
    profileResponse.setMultiDefaultAddressFlag(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER))
        .thenReturn(profileResponse);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("isPrivilegedToReadPrice", true);
    privilegedMap.put("isPrivilegedToReadAvailableStock", true);
    privilegedMap.put("isPrivilegedToReadProductType", true);
    privilegedMap.put("isPrivilegedToReadPickupPoint", true);
    privilegedMap.put("isPrivilegedToReadDisplayBuyable", true);
    privilegedMap.put("isPrivilegedToReadO2O", true);
    privilegedMap.put("isOnlyExternalUser", true);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, true);
    BulkProductResponse productResponse =
        new BulkProductResponse(new ArrayList<List<String>>(), true,
            privilegedMap, new HashMap<String, String>());
    productResponse.setBusinessPartnerCode(BUSINESS_PARTNER);
    String HEADERS = "[\"Blibli Product SKU\"]";
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(HEADERS);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER);
    assertEquals(4, headerList.size());
    List<String> primaryHeader =
        new ObjectMapper().readValue(headerList.get(0), new TypeReference<List<String>>() {});
    assertFalse(primaryHeader.contains(BulkParameters.CNC_STATUS_HEADER));
    assertFalse(primaryHeader.contains(BulkParameters.DELIVERY_STATUS_HEADER));
    ReflectionTestUtils.setField(bulkProductProcessHelper, "cncForWarehouseFeatureSwitch", false);
    ReflectionTestUtils.setField(bulkProductProcessHelper, "cncForWarehouseFeatureSwitch", false);
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }

  @Test
  public void getHeaderList_Cnc1P_withoutAccessibility() throws Exception {
    ReflectionTestUtils.setField(bulkProductProcessHelper, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkProductProcessHelper, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).merchantType(Constant.MERCHANT_TYPE_CM).build());
    profileResponse.setMultiDefaultAddressFlag(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER))
        .thenReturn(profileResponse);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("isPrivilegedToReadPrice", true);
    privilegedMap.put("isPrivilegedToReadAvailableStock", true);
    privilegedMap.put("isPrivilegedToReadProductType", true);
    privilegedMap.put("isPrivilegedToReadPickupPoint", true);
    privilegedMap.put("isPrivilegedToReadO2O", true);
    privilegedMap.put("isOnlyExternalUser", true);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, true);
    BulkProductResponse productResponse =
        new BulkProductResponse(new ArrayList<List<String>>(), true,
            privilegedMap, new HashMap<String, String>());
    productResponse.setBusinessPartnerCode(BUSINESS_PARTNER);
    String HEADERS = "[\"Click & Collect(0= Off, 1 = On)\",\"Pengiriman(0 = Off, 1 = On)\"]";
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(HEADERS);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER);
    assertEquals(4, headerList.size());
    List<String> primaryHeader =
        new ObjectMapper().readValue(headerList.get(0), new TypeReference<List<String>>() {});
    assertTrue(primaryHeader.contains(BulkParameters.CNC_STATUS_HEADER));
    ReflectionTestUtils.setField(bulkProductProcessHelper, "cncForWarehouseFeatureSwitch", false);
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }

  @Test
  public void getHeaderList_Cnc1P_AmphiUser() throws Exception {
    ReflectionTestUtils.setField(bulkProductProcessHelper, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkProductProcessHelper, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).merchantType(Constant.MERCHANT_TYPE_CM).build());
    profileResponse.setMultiDefaultAddressFlag(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER))
        .thenReturn(profileResponse);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("isPrivilegedToReadPrice", true);
    privilegedMap.put("isPrivilegedToReadAvailableStock", true);
    privilegedMap.put("isPrivilegedToReadProductType", true);
    privilegedMap.put("isPrivilegedToReadPickupPoint", true);
    privilegedMap.put("isPrivilegedToReadDisplayBuyable", true);
    privilegedMap.put("isPrivilegedToReadO2O", true);
    privilegedMap.put("isOnlyExternalUser", false);
    privilegedMap.put(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, true);
    BulkProductResponse productResponse =
        new BulkProductResponse(new ArrayList<List<String>>(), true,
            privilegedMap, new HashMap<String, String>());
    productResponse.setBusinessPartnerCode(BUSINESS_PARTNER);
    List<String> headerList = bulkProductProcessHelper.getHeaderList(productResponse);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID,
        BUSINESS_PARTNER);
    assertEquals(4, headerList.size());
//    assertTrue(headerList.contains(BulkParameters.CNC_STATUS_HEADER));
    ReflectionTestUtils.setField(bulkProductProcessHelper, "cncForWarehouseFeatureSwitch", false);
    Mockito.verify(objectMapper, times(4)).writeValueAsString(Mockito.any());
  }

  private void setHeader() {
    List<String> row1 = new ArrayList<>(Arrays
        .asList(StringUtils.EMPTY, "Info produk", StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, "Varian",
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, "Gambar & Video",
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, "Info pengiriman", StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, "Harga & Stok",
            StringUtils.EMPTY, "Pilih Attribut", StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
            StringUtils.EMPTY));
    List<String> row2 = new ArrayList<>(Arrays
        .asList(StringUtils.EMPTY, "Kategori*", StringUtils.EMPTY, StringUtils.EMPTY, "Nama Produk*", "Model/EAN/UPC",
            "Seller SKU", "Deskripsi*", "Keunggulan produk", "Merek*", "Warna*", "Family Colour", "Ukuran", "Variasi",
            "Parent", "Foto-1*", "Foto-2", "Foto-3", "Fotor-4", "Foto-5", "Foto-6", "Foto-7", "Foto-8", "Url Video",
            "Tipe Penanganan*", "Kode Toko/Gudang*", "Panjang (cm)*", "Lebar (cm)*", "Tinggi (cm)*", "Berat (gram)*",
            "Harga Penjualan (Rp)*", "Available Stock*", "Pilih Attribut", "Pilih value", "Pilih Attribut",
            "Pilih value", "Pilih Attribut", "Pilih value", "Pilih Attribut", "Pilih value", "Pilih Attribut",
            "Pilih value"));
    List<String> row3 = new ArrayList<>(Arrays
        .asList(StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
            "(Pilihan)", "(Pilihan)", StringUtils.EMPTY, "(Pilihan)", StringUtils.EMPTY, StringUtils.EMPTY, "(Pilihan)",
            StringUtils.EMPTY, StringUtils.EMPTY, "(Pilihanl)", StringUtils.EMPTY, "(Pilihan)", StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, "(Pilihan) ", StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY));
    List<String> row4 = new ArrayList<>(Arrays.asList(StringUtils.EMPTY, "Pilih kategori dari pilihan yang tersedia.",
        "Pilih subkategori dari pilihan yang tersedia.", "Pilih kategori dari produk yang ingin dijual.",
        "\nMasukkan nama produk. Maksimum 150 karakter.", "Masukkan Unique Product Code (nomor pada barcode).",
        "\nMasukkan detail SKU seller.", "Buat deskripsi produk yang menarik supaya banyak pelanggan melirik.",
        "Ceritakan keunggulan produk Anda yang membedakan dengan produk lainnya.", "Masukkan merek produk Anda.",
        "Masukkan warna produk (jika ada). ", "Masukkan family color (jika ada). ",
        "Masukkan ukuran produk Anda (jika ada). ",
        "Informasi varian lainnya (akan digunakan untuk membuat varian, Anda dapat memasukkan jenis varian apa pun di sini)",
        "Masukkan kode yang sama di sini untuk menghubungkan semua varian.",
        "Masukkan foto produk Anda (Wajib upload min.1 foto, format foto: JPG, JPEG dan PNG, nama foto tidak boleh memiliki karakter khusus).",
        StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
        StringUtils.EMPTY, StringUtils.EMPTY,
        "Anda bisa menambahkan video produk untuk menjelaskan, menggunakan, dan demo produk.",
        "\nMasukkan jenis pengiriman (Reguler; pengiriman oleh seller, BOPIS).",
        "\nMasukkan lokasi toko / gudang dari sheet \"Toko\".", "Panjang produk dalam satuan cm.",
        "Lebar produk dalam satuan cm.", "Tinggi produk dalam satuan cm.", "Berat produk dalam satuan gram.",
        "Tentukan harga jual.", "Silakan isi stok produk. Isi dengan 0 jika tidak tersedia.",
        "Pilih atribut dari daftar yang ada.", "Pilih value untuk atribut terpilih.",
        "Pilih atribut dari daftar yang ada.", "Pilih value untuk atribut terpilih.",
        "Pilih atribut dari daftar yang ada.", "Pilih value untuk atribut terpilih.",
        "Pilih atribut dari daftar yang ada.", "Pilih value untuk atribut terpilih.",
        "Pilih atribut dari daftar yang ada.", "Pilih value untuk atribut terpilih."));
    List<String> data1 = new ArrayList<>(Arrays
        .asList(StringUtils.EMPTY, "Alat Musik", "Aksesoris Alat Musik", "Strings", "Product name", "1", "Seller Sku 1",
            "Description", "USP", "ABC", "Black", "Hitam", StringUtils.EMPTY, StringUtils.EMPTY, "1", "image-1.jpg",
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, "URL Video", "Assigned by Blibli", "PP-3000785", "10", "10", "10",
            "100", "54321", "2", "Fungsi", "Fungsi value", "Lain-lain", "Lain - lian value", "Dimensi Produk",
            "Dimensi value", "Berat", "Berat value", "Material", "0", "Baris : 1. Variasi Invalid attribute value. "));
    List<String> data2 = new ArrayList<>(Arrays
        .asList(StringUtils.EMPTY, "Alat Musik", "Aksesoris Alat Musik", "Strings", "Product name", "1", "Seller Sku 2",
            "Description", "USP", "ABC", "Silver", "Silver", StringUtils.EMPTY, StringUtils.EMPTY, "1", "image-1.jpg",
            "image-2.jpg", StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, "URL Video", "Assigned by Blibli", "PP-3000785", "10", "10", "10",
            "100", "12345", "5", StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY,
            StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY, "1",
            "Baris : 2. Variasi Invalid attribute value. "));
    rawExcelHeaders1 = new ArrayList<>();
    rawExcelHeaders1.add(row1);
    rawExcelHeaders1.add(row2);
    rawExcelHeaders1.add(row3);
    rawExcelHeaders1.add(row4);
    invalidRows1 = new ArrayList<>();
    invalidRows1.add(data1);
    invalidRows1.add(data2);
  }

}
