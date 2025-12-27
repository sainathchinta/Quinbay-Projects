package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.util.GenericBulkHeaders;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;


import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.verifyNoMoreInteractions;

public class ProtectedBrandValidationServiceBeanTest {

  private static final String BRAND = "brand";
  private static final String BRAND_CODE = "brand-code";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String PRODUCT_LEVEL3_PROCESSOR_DIR = "ProductLevel3Processor";
  private static final String FILE_NAME = "GenericExcelWithZipFile";
  private static final String FILE_NAME_2 = "InvalidDataInput1";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String CREATED_BY = "createdBy";
  private static final String UPDATED_BY = "updatedBy";

  private Map<String, String> protectedBrandNameCodeMap;
  private static String STORE_ID =  GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER;
  private static String REQUEST_ID =  GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER;
  private static String CHANNEL_ID =  GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER;
  private static String CLIENT_ID =  GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER;
  private static String USERNAME =  GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER;
  private static BulkProcess bulkProcess;
  private static final BulkUploadErrorCounter bulkUploadErrorCounter = new BulkUploadErrorCounter();
  private static final int brandIndex = 6;
  private static final int rowNumber = 4;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @InjectMocks
  private ProtectedBrandValidationServiceBean protectedBrandValidationService;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);

    protectedBrandNameCodeMap = new HashMap<>();
    protectedBrandNameCodeMap.put(BRAND,BRAND_CODE);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(pcbOutboundService);
  }

  @Test
  public void protectedBrandValidationService() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setRequestId(REQUEST_ID);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    setMdcParameters();
    Mockito.when(pcbOutboundService
      .getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
        bulkProcess.getRequestId(), USERNAME, bulkProcess.getBusinessPartnerCode(),BRAND_CODE))
      .thenReturn(new SimpleBooleanResponse(true));
    protectedBrandValidationService
      .validateProtectedBrandAuthorisation(BRAND, bulkProcess, protectedBrandNameCodeMap);
    Mockito.verify(pcbOutboundService)
      .getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
        bulkProcess.getRequestId(), USERNAME, bulkProcess.getBusinessPartnerCode(),BRAND_CODE);
  }

  @Test
  public void fetchProtectedBrandNameCodeMapTest() {
    Mockito.when(pcbOutboundService.getProtectedBrandList(STORE_ID)).thenReturn(new ArrayList<>());
    protectedBrandValidationService.fetchProtectedBrandNameCodeMap(STORE_ID);
    Mockito.verify(pcbOutboundService).getProtectedBrandList(STORE_ID);
  }

  @Test
  public void protectedBrandTrueValidationService() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setRequestId(REQUEST_ID);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    setMdcParameters();
    boolean result = protectedBrandValidationService
      .validateProtectedBrandAuthorisation(BRAND_CODE, bulkProcess, protectedBrandNameCodeMap);
    Assertions.assertTrue(result);
  }

  @Test
  public void validateInReviewProtectedBrandTrueTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess();
    Mockito.when(pcbOutboundService
      .getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
        bulkProcess.getRequestId(), USERNAME, BRAND_CODE, bulkProcess.getBusinessPartnerCode()))
      .thenReturn(new SimpleBooleanResponse(true));
    protectedBrandValidationService.validateProtectedBrand(getInputRowData(FILE_NAME),bulkProcess
      ,protectedBrandNameCodeMap);
  }

  @Test
  public void validateApprovedProtectedBrandTrueTest() throws Exception {
    setMdcParameters();
    protectedBrandNameCodeMap.put("ABC",BRAND_CODE);
    bulkUploadErrorCounter.setFeature(101);
    BulkProcess bulkProcess = getBulkProcess();
    Mockito.when(pcbOutboundService
      .getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
        bulkProcess.getRequestId(), USERNAME, bulkProcess.getBusinessPartnerCode(),BRAND_CODE))
      .thenReturn(new SimpleBooleanResponse(false));
    protectedBrandValidationService.validateProtectedBrand(getInputRowData(FILE_NAME),bulkProcess
      ,protectedBrandNameCodeMap);
    Mockito.verify(pcbOutboundService).getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
      bulkProcess.getRequestId(), USERNAME, bulkProcess.getBusinessPartnerCode(),BRAND_CODE);
  }

  @Test
  public void validateEmptyBrandTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess();
    Mockito.when(pcbOutboundService
      .getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
        bulkProcess.getRequestId(), USERNAME, BRAND_CODE, bulkProcess.getBusinessPartnerCode()))
      .thenReturn(new SimpleBooleanResponse(true));
    protectedBrandValidationService.validateProtectedBrand(getInputRowData(FILE_NAME_2), bulkProcess,
        protectedBrandNameCodeMap);
  }

  @Test
  public void validateProtectedBrandFalseTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess();
    protectedBrandNameCodeMap.put("ABC",BRAND_CODE);
    setMdcParameters();
    Mockito.when(pcbOutboundService
      .getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
        bulkProcess.getRequestId(), USERNAME, bulkProcess.getBusinessPartnerCode(),BRAND_CODE))
      .thenReturn(new SimpleBooleanResponse(false));
    protectedBrandValidationService.validateProtectedBrand(getInputRowData(FILE_NAME), bulkProcess,
        protectedBrandNameCodeMap);
    Mockito.verify(pcbOutboundService)
      .getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
        bulkProcess.getRequestId(), USERNAME, bulkProcess.getBusinessPartnerCode(),BRAND_CODE);
  }

  private void setMdcParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
      GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
      GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
      GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
      GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
      GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
  }

  private static Map<String, Object> getInputRowData(String fileName) throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
      .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + fileName);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    LinkedHashMap<String, Object> dataMap = objectMapper.readValue(inputStream, typeRef);
    dataMap.put(GenericBulkHeaders.ROW_NUMBER, 10);
    return dataMap;
  }

  private static Map<String, Object> getInputRowDataMap(String fileName) throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + fileName);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    LinkedHashMap<String, Object> dataMap = objectMapper.readValue(inputStream, typeRef);
    return dataMap;
  }

  private static BulkProcess getBulkProcess() {
    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_GENERIC.name());
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedBy(CREATED_BY);
    bulkProcess.setRequestId(REQUEST_ID);
    bulkProcess.setUpdatedBy(UPDATED_BY);
    bulkProcess.setTotalCount(1);
    bulkProcess.setSuccessCount(1);
    bulkProcess.setInternationalMerchant(false);
    bulkProcess.setDescription("general_template (3) copy 3.xlsm. proses validasi berlangsung");
    return bulkProcess;
  }

  @Test
  public void validateInReviewProtectedBrandCnTrueTest() throws Exception {
    BulkProcess bulkProcess = getBulkProcess();
    Mockito.when(pcbOutboundService.getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
            bulkProcess.getRequestId(), USERNAME, BRAND_CODE, bulkProcess.getBusinessPartnerCode()))
        .thenReturn(new SimpleBooleanResponse(true));
    protectedBrandValidationService.validateProtectedBrandForCn(getInputRowDataMap(FILE_NAME), bulkProcess,
        protectedBrandNameCodeMap);
  }

  @Test
  public void validateApprovedProtectedBrandCnTrueCleanValueTest() throws Exception {
    setMdcParameters();
    protectedBrandNameCodeMap.put("Description", BRAND_CODE);
    BulkProcess bulkProcess = getBulkProcess();
    Mockito.when(pcbOutboundService.getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
            bulkProcess.getRequestId(), USERNAME, bulkProcess.getBusinessPartnerCode(), BRAND_CODE))
        .thenReturn(new SimpleBooleanResponse(false));
    Map<String, Object> map = getInputRowDataMap(FILE_NAME);
    map.put(Constant.BRAND, "brand");
    protectedBrandValidationService.validateProtectedBrandForCn(map, bulkProcess,
        protectedBrandNameCodeMap);
    Mockito.verify(pcbOutboundService)
        .getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID, bulkProcess.getRequestId(), USERNAME,
            bulkProcess.getBusinessPartnerCode(), BRAND_CODE);
  }

  @Test
  public void validateApprovedProtectedBrandCnTrueTest() throws Exception {
    setMdcParameters();
    protectedBrandNameCodeMap.put("Description", BRAND_CODE);
    BulkProcess bulkProcess = getBulkProcess();
    Mockito.when(pcbOutboundService.getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
            bulkProcess.getRequestId(), USERNAME, bulkProcess.getBusinessPartnerCode(), BRAND_CODE))
        .thenReturn(new SimpleBooleanResponse(false));
    Map<String, Object> map = getInputRowDataMap(FILE_NAME);
    map.put(Constant.BRAND, "brand (IN_REVIEW)");
    protectedBrandValidationService.validateProtectedBrandForCn(map, bulkProcess,
        protectedBrandNameCodeMap);
    Mockito.verify(pcbOutboundService)
        .getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID, bulkProcess.getRequestId(), USERNAME,
            bulkProcess.getBusinessPartnerCode(), BRAND_CODE);
  }

  @Test
  public void validateApprovedProtectedBrandCnTrueEmptyValueTest() throws Exception {
    setMdcParameters();
    protectedBrandNameCodeMap.put("Description", BRAND_CODE);
    BulkProcess bulkProcess = getBulkProcess();
    Mockito.when(pcbOutboundService.getBrandAuthorisation(bulkProcess.getStoreId(), CHANNEL_ID, CLIENT_ID,
            bulkProcess.getRequestId(), USERNAME, bulkProcess.getBusinessPartnerCode(), BRAND_CODE))
        .thenReturn(new SimpleBooleanResponse(false));
    Map<String, Object> map = getInputRowDataMap(FILE_NAME);
    map.put(Constant.BRAND, "");
    Assertions.assertTrue(protectedBrandValidationService.validateProtectedBrandForCn(map, bulkProcess,
        protectedBrandNameCodeMap));
  }

}
