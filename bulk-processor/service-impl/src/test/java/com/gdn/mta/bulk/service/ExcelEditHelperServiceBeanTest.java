package com.gdn.mta.bulk.service;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadDTO;
import com.gdn.mta.bulk.entity.UnifiedBulkDownloadEvent;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;

import static org.mockito.Mockito.verifyNoMoreInteractions;


public class ExcelEditHelperServiceBeanTest {

  @InjectMocks
  private ExcelEditHelperServiceImpl excelEditHelperService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private PickupPointService pickupPointService;

  private ProfileResponse profileResponse;
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BPCODE";
  private static final String DEFAULT_PP_CODE = "PPCODE";
  private static final String BRAND = "Brand";
  private static final String BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM = "Blibli-mass-upload-template.xlsm";
  private static final ClassLoader CLASS_LOADER = ClassLoader.getSystemClassLoader();
  private static final String BASE_DIRECTORY =
    CLASS_LOADER.getResource(StringUtils.EMPTY).getPath();
  private static final String UNIFIED_BASE_DIRECTORY = BASE_DIRECTORY + "/ExcelTemplate/";
  private static final String PICKUP_POINT_NAME_DELIMITER = "||";
  private List<PickupPointDTO> pickupPointDTOList;
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse1;
  private UnifiedBulkDownloadEvent unifiedBulkDownloadEvent;
  private PickupPointResponse pickupPointResponse;
  /*private static final String BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM =
    "Blibli-mass-upload-template.xlsm,Blibli-mass-upload-template-delivery.xlsm,Blibli-mass-upload-template-cnc.xlsm";*/

  private ShippingTypeEligibility shippingTypeEligibility = new ShippingTypeEligibility();

  @BeforeEach
  public void init() throws UnsupportedEncodingException, ApplicationException {
    MockitoAnnotations.initMocks(this);
    profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setOfflineToOnlineFlag(true);
    companyDTO.setInventoryFulfillment("BL");
    profileResponse.setCompany(companyDTO);
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointDTOList = new ArrayList<>();

    predefinedAllowedAttributeValueResponse1 = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse1.setValue(BRAND);
    unifiedBulkDownloadEvent = new UnifiedBulkDownloadEvent();
    unifiedBulkDownloadEvent.setDownloadStatus("IN_PROGRESS");
    unifiedBulkDownloadEvent.setBrandUpdated(true);
    unifiedBulkDownloadEvent.setPickupPointUpdated(true);
    unifiedBulkDownloadEvent.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    unifiedBulkDownloadEvent.setLastDownloadedTime(new Date(25));

    PickupPointDTO pickupPointDto = new PickupPointDTO();
    pickupPointDto.setCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointDTOList.add(pickupPointDto);
    profileResponse.setPickupPoints(pickupPointDTOList);

    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(DEFAULT_PP_CODE);

    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
  }

  @AfterEach
  public void tearDown() {
    profileResponse = new ProfileResponse();
    verifyNoMoreInteractions(fileStorageService);
  }


  @Test
  public void getProductUnifiedTemplateTest() throws Exception {
    String fileDownloadPath = "defaultPath/bulkDownload";
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(DEFAULT_PP_CODE);
    pickupPointResponse.setFbbActivated(true);

    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    profileResponse.setMultiDefaultAddressFlag(true);
    File file =
      new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    byte[] fileContent = Files.readAllBytes(file.toPath());
    Mockito.when(fileStorageService.downloadGenericTemplateFile(BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM))
      .thenReturn(fileContent);
    Mockito.when(fileStorageService.createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any()))
      .thenReturn(fileDownloadPath);
    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = excelEditHelperService.getProductUnifiedTemplate(profileResponse,
        Arrays.asList(predefinedAllowedAttributeValueResponse1), BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM, true,
        unifiedBulkDownloadEvent, false, false, PICKUP_POINT_NAME_DELIMITER, shippingTypeEligibility,
        BulkInternalProcessType.GENERIC_FILE_GENERATION, Collections.emptySet());
    Mockito.verify(fileStorageService)
      .downloadGenericTemplateFile(BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    Mockito.verify(fileStorageService).createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any());
    Assertions.assertEquals(unifiedBulkDownloadDTO.getFilePath(), fileDownloadPath);
  }

  @Test
  public void getProductUnifiedTemplateFilteredPickupPointCodesTest() throws Exception {
    String fileDownloadPath = "defaultPath/bulkDownload";
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(DEFAULT_PP_CODE);
    pickupPointResponse.setFbbActivated(true);

    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    profileResponse.setMultiDefaultAddressFlag(true);
    File file =
        new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    byte[] fileContent = Files.readAllBytes(file.toPath());
    Mockito.when(fileStorageService.downloadGenericTemplateFile(BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM))
        .thenReturn(fileContent);
    Mockito.when(fileStorageService.createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.any(BulkInternalProcessType.class), Mockito.any())).thenReturn(fileDownloadPath);
    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = excelEditHelperService.getProductUnifiedTemplate(profileResponse,
        Arrays.asList(predefinedAllowedAttributeValueResponse1), BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM, true,
        unifiedBulkDownloadEvent, false, false, PICKUP_POINT_NAME_DELIMITER, shippingTypeEligibility,
        BulkInternalProcessType.GENERIC_FILE_GENERATION, Collections.singleton(DEFAULT_PP_CODE));
    Mockito.verify(fileStorageService)
        .downloadGenericTemplateFile(BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    Mockito.verify(fileStorageService).createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.any(BulkInternalProcessType.class), Mockito.any());
    Assertions.assertEquals(unifiedBulkDownloadDTO.getFilePath(), fileDownloadPath);
  }

  @Test
  public void getProductUnifiedTemplateGlobalFalseTest() throws Exception {
    shippingTypeEligibility.setEligibleForBopisProduct(true);
    shippingTypeEligibility.setEligibleForBigProduct(true);
    String fileDownloadPath = "defaultPath/bulkDownload";
    File file =
      new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    byte[] fileContent = Files.readAllBytes(file.toPath());
    Mockito.when(fileStorageService.downloadGenericTemplateFile(
      DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH + Constant.GENERAL_TEMPLATE))
      .thenReturn(fileContent);
    Mockito.when(fileStorageService.createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any()))
      .thenReturn(fileDownloadPath);
    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = excelEditHelperService.getProductUnifiedTemplate(profileResponse,
        Arrays.asList(predefinedAllowedAttributeValueResponse1), BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM,
        false, unifiedBulkDownloadEvent, true, false, PICKUP_POINT_NAME_DELIMITER,
        shippingTypeEligibility, BulkInternalProcessType.GENERIC_FILE_GENERATION,
        Collections.emptySet());
    Mockito.verify(fileStorageService).downloadGenericTemplateFile(
      DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH + Constant.GENERAL_TEMPLATE);
    Mockito.verify(fileStorageService).createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any());
    Assertions.assertEquals(unifiedBulkDownloadDTO.getFilePath(), fileDownloadPath);
  }

  @Test
  public void getProductUnifiedTemplateGlobalFalseBundlingTemplateTest() throws Exception {
    shippingTypeEligibility.setEligibleForBopisProduct(true);
    shippingTypeEligibility.setEligibleForBigProduct(true);
    ReflectionTestUtils.setField(excelEditHelperService, "gcsMassBundlingTemplatePath", "bundling");
    String fileDownloadPath = "defaultPath/bulkDownload";
    File file =
        new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    byte[] fileContent = Files.readAllBytes(file.toPath());
    Mockito.when(fileStorageService.downloadGenericTemplateFile(
             "bundling" + Constant.SLASH + DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH + Constant.GENERAL_TEMPLATE))
        .thenReturn(fileContent);
    Mockito.when(fileStorageService.createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any()))
        .thenReturn(fileDownloadPath);
    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = excelEditHelperService.getProductUnifiedTemplate(profileResponse,
        Arrays.asList(predefinedAllowedAttributeValueResponse1), BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM,
        false, unifiedBulkDownloadEvent, true, false, PICKUP_POINT_NAME_DELIMITER,
        shippingTypeEligibility, BulkInternalProcessType.GENERIC_BUNDLING_FILE_GENERATION,
        Collections.emptySet());
    Mockito.verify(fileStorageService).downloadGenericTemplateFile(
        "bundling" + Constant.SLASH + DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH + Constant.GENERAL_TEMPLATE);
    Mockito.verify(fileStorageService).createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any());
    Assertions.assertEquals(fileDownloadPath, unifiedBulkDownloadDTO.getFilePath());
  }

  @Test
  public void getProductUnifiedTemplateGlobalFalseBundlingTemplateBulkExcelVersionEnTest() throws Exception {
    shippingTypeEligibility.setEligibleForBopisProduct(true);
    shippingTypeEligibility.setEligibleForBigProduct(true);
    ReflectionTestUtils.setField(excelEditHelperService, "gcsMassBundlingTemplatePath", "bundling");
    ReflectionTestUtils.setField(excelEditHelperService, "bulkExcelVersioningEn", true);
    ReflectionTestUtils.setField(excelEditHelperService, "bulkGenericExcelVersion", "1.1");
    String fileDownloadPath = "defaultPath/bulkDownload";
    File file = new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    byte[] fileContent = Files.readAllBytes(file.toPath());
    Mockito.when(fileStorageService.downloadGenericTemplateFile(
        "bundling" + Constant.SLASH + DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH + Constant.NEW_GENERAL_TEMPLATE
            + Constant.UNDERSCORE + "1.1" + Constant.NEW_GENERAL_TEMPLATE_EXTENSION)).thenReturn(fileContent);
    Mockito.when(
        fileStorageService.createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class),
            Mockito.any())).thenReturn(fileDownloadPath);
    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = excelEditHelperService.getProductUnifiedTemplate(profileResponse,
        Arrays.asList(predefinedAllowedAttributeValueResponse1), BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM, false,
        unifiedBulkDownloadEvent, true, false, PICKUP_POINT_NAME_DELIMITER, shippingTypeEligibility,
        BulkInternalProcessType.GENERIC_BUNDLING_FILE_GENERATION, new HashSet<>());
    Mockito.verify(fileStorageService).downloadGenericTemplateFile(

        "bundling" + Constant.SLASH + DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH + Constant.NEW_GENERAL_TEMPLATE
            + Constant.UNDERSCORE + "1.1" + Constant.NEW_GENERAL_TEMPLATE_EXTENSION);
    Mockito.verify(fileStorageService)
        .createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any());
    Assertions.assertEquals(fileDownloadPath, unifiedBulkDownloadDTO.getFilePath());
  }

  @Test
  public void getProductUnifiedTemplateGlobalFalseBundlingTemplateBulkExcelVersionEnAndInstoreVersionTest() throws Exception {
    shippingTypeEligibility.setEligibleForBopisProduct(true);
    shippingTypeEligibility.setEligibleForBigProduct(true);
    ReflectionTestUtils.setField(excelEditHelperService, "gcsMassBundlingTemplatePath", "bundling");
    ReflectionTestUtils.setField(excelEditHelperService, "bulkExcelVersioningEn", true);
    ReflectionTestUtils.setField(excelEditHelperService, "bulkGenericExcelVersion", "1.1");
    ReflectionTestUtils.setField(excelEditHelperService, "bulkGenericInstoreExcelVersion", "1.2");
    ReflectionTestUtils.setField(excelEditHelperService, "instoreNewFlowEnabled", true);
    String fileDownloadPath = "defaultPath/bulkDownload";
    File file =
        new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    byte[] fileContent = Files.readAllBytes(file.toPath());
    Mockito.when(fileStorageService.downloadGenericTemplateFile(
        "bundling" + Constant.SLASH + DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH
            + Constant.NEW_GENERAL_TEMPLATE + Constant.UNDERSCORE + "1.2"
            + Constant.NEW_GENERAL_TEMPLATE_EXTENSION)).thenReturn(fileContent);
    Mockito.when(fileStorageService.createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.any(BulkInternalProcessType.class), Mockito.any())).thenReturn(fileDownloadPath);
    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = excelEditHelperService.getProductUnifiedTemplate(profileResponse,
        Arrays.asList(predefinedAllowedAttributeValueResponse1), BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM,
        false, unifiedBulkDownloadEvent, true, false, PICKUP_POINT_NAME_DELIMITER,
        shippingTypeEligibility, BulkInternalProcessType.GENERIC_BUNDLING_FILE_GENERATION,
        new HashSet<>());
    Mockito.verify(fileStorageService).downloadGenericTemplateFile(

        "bundling" + Constant.SLASH + DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH
            + Constant.NEW_GENERAL_TEMPLATE + Constant.UNDERSCORE + "1.2"
            + Constant.NEW_GENERAL_TEMPLATE_EXTENSION);
    Mockito.verify(fileStorageService)
        .createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.any(BulkInternalProcessType.class), Mockito.any());
    Assertions.assertEquals(fileDownloadPath, unifiedBulkDownloadDTO.getFilePath());
  }

  @Test
  public void getProductUnifiedTemplateEmptyPickupPointListTest() throws Exception {
    shippingTypeEligibility.setEligibleForBopisProduct(false);
    shippingTypeEligibility.setEligibleForBigProduct(false);
    profileResponse.setPickupPoints(new ArrayList<>());
    String fileDownloadPath = "defaultPath/bulkDownload";
    File file =
      new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    byte[] fileContent = Files.readAllBytes(file.toPath());
    Mockito.when(fileStorageService.downloadGenericTemplateFile(
      DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH + Constant.GENERAL_TEMPLATE))
      .thenReturn(fileContent);
    Mockito.when(fileStorageService.createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any()))
      .thenReturn(fileDownloadPath);
    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = excelEditHelperService.getProductUnifiedTemplate(profileResponse,
        new ArrayList<>(), BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM, false, unifiedBulkDownloadEvent, true,
        false, PICKUP_POINT_NAME_DELIMITER, shippingTypeEligibility,
        BulkInternalProcessType.GENERIC_FILE_GENERATION, Collections.emptySet());
    Mockito.verify(fileStorageService).downloadGenericTemplateFile(
      DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH + Constant.GENERAL_TEMPLATE);
    Mockito.verify(fileStorageService).createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any());
    Assertions.assertEquals(fileDownloadPath, unifiedBulkDownloadDTO.getFilePath());
  }

  @Test
  public void getProductUnifiedTemplateEmptyBrandTest() throws Exception {
    shippingTypeEligibility.setEligibleForBopisProduct(true);
    shippingTypeEligibility.setEligibleForBigProduct(true);
    profileResponse.setPickupPoints(new ArrayList<>());
    String fileDownloadPath = "defaultPath/bulkDownload";
    File file =
      new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    byte[] fileContent = Files.readAllBytes(file.toPath());
    Mockito.when(fileStorageService.downloadGenericTemplateFile(
      DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH + Constant.GENERAL_TEMPLATE))
      .thenReturn(fileContent);
    Mockito.when(fileStorageService.createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any()))
      .thenReturn(fileDownloadPath);
    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = excelEditHelperService.getProductUnifiedTemplate(profileResponse,
        new ArrayList<>(), BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM, false, unifiedBulkDownloadEvent, true,
        false, PICKUP_POINT_NAME_DELIMITER, shippingTypeEligibility,
        BulkInternalProcessType.GENERIC_FILE_GENERATION, Collections.emptySet());
    Mockito.verify(fileStorageService).downloadGenericTemplateFile(
      DEFAULT_BUSINESS_PARTNER_CODE + Constant.SLASH + Constant.GENERAL_TEMPLATE);
    Mockito.verify(fileStorageService).createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any());
    Assertions.assertEquals(unifiedBulkDownloadDTO.getFilePath(), fileDownloadPath);
  }

  @Test
  public void downloadProductUnifiedTemplateFirstRequestTest() throws Exception {
    shippingTypeEligibility.setEligibleForBopisProduct(true);
    shippingTypeEligibility.setEligibleForBigProduct(true);
    profileResponse.setPickupPoints(new ArrayList<>());
    String fileDownloadPath = "defaultPath/bulkDownload";
    File file =
      new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    byte[] fileContent = Files.readAllBytes(file.toPath());
    Mockito.when(fileStorageService.downloadGenericTemplateFile(BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM))
      .thenReturn(fileContent);
    Mockito.when(fileStorageService.createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any()))
      .thenReturn(fileDownloadPath);
    UnifiedBulkDownloadDTO unifiedBulkDownloadDTO = excelEditHelperService.getProductUnifiedTemplate(profileResponse,
        Arrays.asList(predefinedAllowedAttributeValueResponse1), BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM,
        true, unifiedBulkDownloadEvent, true, false, PICKUP_POINT_NAME_DELIMITER,
        shippingTypeEligibility, BulkInternalProcessType.GENERIC_BUNDLING_FILE_GENERATION,
        Collections.emptySet());
    Mockito.verify(fileStorageService)
      .downloadGenericTemplateFile(BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    Mockito.verify(fileStorageService).createGenericBulkFile(Mockito.anyString(), Mockito.anyBoolean(), Mockito.any(BulkInternalProcessType.class), Mockito.any());
    Assertions.assertEquals(unifiedBulkDownloadDTO.getFilePath(), fileDownloadPath);
  }
}
