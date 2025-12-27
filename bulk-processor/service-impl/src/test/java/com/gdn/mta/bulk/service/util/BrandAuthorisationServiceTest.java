package com.gdn.mta.bulk.service.util;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.BrandAuthAddRequestData;
import com.gdn.mta.bulk.models.BrandAuthDeleteRequestData;
import com.gdn.mta.bulk.models.BrandAuthorizationEmailTemplate;
import com.gdn.mta.bulk.service.BrandAuthorisationServiceImpl;
import com.gdn.mta.bulk.service.FileStorageService;
import com.gdn.mta.bulk.service.InternalProcessService;
import com.gdn.mta.bulk.service.MailDeliveryService;

public class BrandAuthorisationServiceTest {

  private static final String BRAND_AUTH_DELETE_COMPLETED_TEMPLATE_ID = "BRAND_AUTH_DELETE_COMPLETED_TEMPLATE_ID";
  private static final String BRAND_AUTH_DELETE_COMPLETED_TEMPLATE = "Brand authorization has been deleted";
  private static final String BRAND_AUTH_DELETE_FAILED_TEMPLATE_ID = "BRAND_AUTH_DELETE_FAILED_TEMPLATE_ID";
  private static final String BRAND_AUTH_DELETE_FAILED_TEMPLATE = "Failed to delete brand authorization";
  private static final String BRAND_AUTH_DELETE_PARTIALLY_COMPLETED_TEMPLATE_ID =
      "BRAND_AUTH_DELETE_PARTIALLY_COMPLETED_TEMPLATE_ID";
  private static final String BRAND_AUTH_DELETE_PARTIALLY_COMPLETED_TEMPLATE =
      "Brand authorization has partially deleted";
  @InjectMocks
  private BrandAuthorisationServiceImpl brandAuthorisationService;

  @Mock
  private InternalProcessService internalProcessService;
  @Mock
  private FileStorageService fileStorageService;
  @Mock
  private MailDeliveryService mailDeliveryService;
  @Mock
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<BulkInternalProcess> bulkInternalProcessArgumentCaptor;

  private static final String STORE_ID =  "STORE_ID";
  private static final String ID =  "ID";
  private static final String DELETE_BRAND_AUTHORISATION =  "DELETE_BRAND_AUTHORISATION";
  private static final String ERROR_MESSAGE = "ERROR_MESSAGE";
  private static final String COMPLETED = "COMPLETED";
  private static final String PARTIAL_COMPLETED = "PARTIAL_COMPLETED";
  private static final String FAILED = "FAILED";
  private static final String USERNAME = "USERNAME";

  private BulkInternalProcess bulkInternalProcess;
  private BulkInternalProcessData bulkInternalProcessData1;
  private BulkInternalProcessData bulkInternalProcessData2;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(DELETE_BRAND_AUTHORISATION);
    bulkInternalProcess.setId(ID);
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setInternalProcessRequestCode(ID);
    bulkInternalProcess.setCreatedBy(USERNAME);
    bulkInternalProcess.setFileName(USERNAME);
    bulkInternalProcess.setErrorFilePath(USERNAME);
    bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData2 = new BulkInternalProcessData();
    ReflectionTestUtils.setField(brandAuthorisationService,"gcsBrandAuthErrorPath", ID);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelCompletedTest() throws Exception {
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, ID)).thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    brandAuthorisationService.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(COMPLETED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(2, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(0, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelPartiallyCompletedTest() throws Exception {
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, ID)).thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    brandAuthorisationService.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(PARTIAL_COMPLETED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelFailedTest() throws Exception {
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData2.setErrorMessage(ERROR_MESSAGE);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, ID)).thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    brandAuthorisationService.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService).getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(FAILED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(0, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(2, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
  }

  @Test
  public void processFinalStatusForBrandAuthDeleteSuccessTest() throws Exception {
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate =
        brandAuthorisationService.processFinalStatusForBrandAuthDelete(bulkInternalProcess, new HashMap<>(), 0,
            new String(), new BrandAuthorizationEmailTemplate());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
    Assertions.assertEquals(BRAND_AUTH_DELETE_COMPLETED_TEMPLATE_ID, brandAuthorizationEmailTemplate.getEmailTemplateId());
    Assertions.assertEquals(BRAND_AUTH_DELETE_COMPLETED_TEMPLATE, brandAuthorizationEmailTemplate.getEmailTemplate());
    Assertions.assertEquals(0, brandAuthorizationEmailTemplate.getSuccessCount().intValue());
  }

  @Test
  public void processFinalStatusForBrandAuthDeleteFailedTest() throws Exception {
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcess.setFileName(STORE_ID);
    bulkInternalProcess.setErrorFilePath(STORE_ID);
    Map<Integer, BulkInternalProcessData> map = new HashMap<>();
    map.put(1, bulkInternalProcessData1);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any())).thenReturn(bulkInternalProcess);
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate =
        brandAuthorisationService.processFinalStatusForBrandAuthDelete(bulkInternalProcess, map, 1, new String(),
            new BrandAuthorizationEmailTemplate());
    Mockito.verify(fileStorageService)
        .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.any());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
    Assertions.assertEquals(BRAND_AUTH_DELETE_FAILED_TEMPLATE_ID, brandAuthorizationEmailTemplate.getEmailTemplateId());
    Assertions.assertEquals(BRAND_AUTH_DELETE_FAILED_TEMPLATE, brandAuthorizationEmailTemplate.getEmailTemplate());
    Assertions.assertEquals(0, brandAuthorizationEmailTemplate.getSuccessCount().intValue());
  }

  @Test
  public void processFinalStatusForBrandAuthDeletePartialTest() throws Exception {
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setFileName(STORE_ID);
    bulkInternalProcess.setErrorFilePath(STORE_ID);
    Map<Integer, BulkInternalProcessData> map = new HashMap<>();
    map.put(1, bulkInternalProcessData1);
    map.put(2, bulkInternalProcessData2);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any())).thenReturn(bulkInternalProcess);
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate =
        brandAuthorisationService.processFinalStatusForBrandAuthDelete(bulkInternalProcess, map, 1, new String(),
            new BrandAuthorizationEmailTemplate());
    Mockito.verify(fileStorageService)
        .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.any());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
    Assertions.assertEquals(BRAND_AUTH_DELETE_PARTIALLY_COMPLETED_TEMPLATE_ID,
        brandAuthorizationEmailTemplate.getEmailTemplateId());
    Assertions.assertEquals(BRAND_AUTH_DELETE_PARTIALLY_COMPLETED_TEMPLATE,
        brandAuthorizationEmailTemplate.getEmailTemplate());
    Assertions.assertEquals(1, brandAuthorizationEmailTemplate.getSuccessCount().intValue());
  }

  @Test
  public void processFinalStatusForBrandAuthAddSuccessTest() throws Exception {
    brandAuthorisationService.processFinalStatusForBrandAuthAdd(bulkInternalProcess, new HashMap<>(), 0,
        new String(), new BrandAuthorizationEmailTemplate());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
  }

  @Test
  public void processFinalStatusForBrandAuthAddFailedTest() throws Exception {
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcess.setFileName(STORE_ID);
    bulkInternalProcess.setErrorFilePath(STORE_ID);
    Map<Integer, BulkInternalProcessData> map = new HashMap<>();
    map.put(1, bulkInternalProcessData1);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any())).thenReturn(bulkInternalProcess);
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate =
        brandAuthorisationService.processFinalStatusForBrandAuthAdd(bulkInternalProcess, map, 1, new String(),
            new BrandAuthorizationEmailTemplate());
    Mockito.verify(fileStorageService)
        .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.any());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
  }

  @Test
  public void processFinalStatusForBrandAuthAddPartialTest() throws Exception {
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setFileName(STORE_ID);
    bulkInternalProcess.setErrorFilePath(STORE_ID);
    Map<Integer, BulkInternalProcessData> map = new HashMap<>();
    map.put(1, bulkInternalProcessData1);
    map.put(2, bulkInternalProcessData2);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any())).thenReturn(bulkInternalProcess);
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate =
        brandAuthorisationService.processFinalStatusForBrandAuthAdd(bulkInternalProcess, map, 1, new String(),
            new BrandAuthorizationEmailTemplate());
    Mockito.verify(fileStorageService)
        .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.any());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkBrandAuthAddTest() throws Exception {
    BrandAuthAddRequestData brandAuthAddRequestData = BrandAuthAddRequestData.builder().excelRowNumber(1).build();
    String message = new ObjectMapper().writeValueAsString(brandAuthAddRequestData);
    bulkInternalProcessData1.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcessData1.setData(message);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(objectMapper.readValue(message, BrandAuthAddRequestData.class)).thenReturn(brandAuthAddRequestData);
    brandAuthorisationService.setFinalStatusAndGenerateFailedExcelForBulkBrandAuth(STORE_ID, bulkInternalProcess);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkBrandAuthStatusTest() throws Exception {
    bulkInternalProcessData1.setStatus(PARTIAL_COMPLETED);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(Collections.singletonList(bulkInternalProcessData1));
    brandAuthorisationService.setFinalStatusAndGenerateFailedExcelForBulkBrandAuth(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkBrandAuthNullTest() throws Exception {
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(null);
    brandAuthorisationService.setFinalStatusAndGenerateFailedExcelForBulkBrandAuth(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkBrandAuthDeleteTest() throws Exception {
    BrandAuthDeleteRequestData brandAuthDeleteRequestData =
        BrandAuthDeleteRequestData.builder().excelRowNumber(1).build();
    String message = new ObjectMapper().writeValueAsString(brandAuthDeleteRequestData);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    bulkInternalProcessData1.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcessData1.setData(message);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(objectMapper.readValue(message, BrandAuthDeleteRequestData.class))
        .thenReturn(brandAuthDeleteRequestData);
    brandAuthorisationService.setFinalStatusAndGenerateFailedExcelForBulkBrandAuth(STORE_ID, bulkInternalProcess);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
  }

}
