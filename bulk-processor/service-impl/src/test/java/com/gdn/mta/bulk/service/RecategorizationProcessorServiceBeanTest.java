package com.gdn.mta.bulk.service;

import static org.mockito.Mockito.times;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gda.mta.product.dto.BulkDataForRecategorizationRequest;
import com.gda.mta.product.dto.CategoryProductSkuMappingRequest;
import com.gda.mta.product.dto.CategoryUserMappingRequest;
import com.gda.mta.product.dto.ProductSkuToSalesCatalogMappingRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.notification.enumeration.NotificationType;

/**
 * Created by hardikbohra on 07/06/18.
 */
public class RecategorizationProcessorServiceBeanTest {

  public static final String STORE_ID = "storeId";
  public static final String REQUEST_ID = "requestId";
  public static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  public static final String BULK_PARTNER_CODE = "bulkPartnerCode";
  public static final String DEFAULT_EXCHANGE_3 = "DEFAULT_EXCHANGE_3";
  public static final String DEFAULT_ROUTING_2 = "DEFAULT_ROUTING_2";
  private static final String BULK_CREATE_EVENT = "com.gdn.mta.bulk.create";
  private static final String EVENT = "event1";

  @InjectMocks
  private RecategorizationProcessorServiceBean serviceBean;

  @Mock
  private BulkProcessRepository bulkProcessRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private SystemParameter systemParameter;

  @Mock
  private NotificationService notificationService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(serviceBean, "batchSize", 50);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkProcessRepository);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(systemParameter);
    Mockito.verifyNoMoreInteractions(notificationService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void preProcessTest() throws Exception {
    Map<String, String> files = getFile();
    Map<String, String> arguments = getArgs();
    Mockito.when(kafkaTopicProperties.getBulkCreateEvent()).thenReturn(BULK_CREATE_EVENT);
    serviceBean.preProcess(STORE_ID, REQUEST_ID, BulkProcessType.RECATEGORIZATION.toString(), BUSINESS_PARTNER_CODE,
        files, arguments, UUID.randomUUID().toString(), "");
    Mockito.verify(bulkProcessRepository).saveAndFlush(Mockito.any(BulkProcess.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq(BULK_CREATE_EVENT), Mockito.any(BulkProcessQueue.class));
    Mockito.verify(kafkaTopicProperties).getBulkCreateEvent();
  }

  @Test
  public void preProcessTest_WhenFileIsEmpty() throws Exception {
    Map<String, String> files = getFileBlank();
    Map<String, String> arguments = getArgs();
    try {
      serviceBean.preProcess(STORE_ID, REQUEST_ID, BulkProcessType.RECATEGORIZATION.toString(),
          BUSINESS_PARTNER_CODE, files, arguments, UUID.randomUUID().toString(), "");
    } catch (Exception ex) {
      Assertions.assertEquals(0, files.get("xls").length());
    }
  }

  @Test
  public void processTest_bulkProcessIsNull() throws Exception {
    try {
      BulkProcessQueue bulkProcessQueue = new BulkProcessQueue(STORE_ID, BUSINESS_PARTNER_CODE, BulkProcessType
          .RECATEGORIZATION.getValue(), getArgs());
      serviceBean.process(bulkProcessQueue);
    } catch (Exception ex) {
      Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
          BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void processTest_fileNotFound() throws Exception {
    BulkProcess bulkProcess = createBulkProcess(STORE_ID, REQUEST_ID, BULK_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), BUSINESS_PARTNER_CODE, "excelName");
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue(STORE_ID, BUSINESS_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), getArgs());
    try {
      Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
          BUSINESS_PARTNER_CODE)).thenReturn(bulkProcess);
      serviceBean.process(bulkProcessQueue);
    } catch (Exception ex) {
      Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
          BUSINESS_PARTNER_CODE);
      Mockito.verify(bulkProcessRepository).save(Mockito.eq(bulkProcess));
    }
  }

  @Test
  public void processTest() throws Exception {
    BulkProcess bulkProcess = createBulkProcess(STORE_ID, REQUEST_ID, BULK_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), BUSINESS_PARTNER_CODE, "excelName");
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue(STORE_ID, BUSINESS_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), getArgs());
    Map<String, String> files = this.getFile();
    byte[] excelFile = Base64.decodeBase64(files.get("xls"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator +
        bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_EXCEL, excelFile);

    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(bulkProcess);
    serviceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE);
    Mockito.verify(bulkProcessRepository, times(2)).save(Mockito.eq(bulkProcess));
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToBusinessPartnerMappingEvent()), Mockito.any(CategoryUserMappingRequest.class));
    Mockito.verify(kafkaProducer, times(3)).send(
        Mockito.eq(kafkaTopicProperties.getCategoryToProductSkuMappingEvent()), Mockito.any(CategoryProductSkuMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito
        .eq(kafkaTopicProperties.getProductSkuToSalesCatalogMappingEvent()), Mockito.any(ProductSkuToSalesCatalogMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToProductCodeMappingEvent()), Mockito.any(BulkDataForRecategorizationRequest.class));
    Mockito.verify(notificationService).sendNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(), false);
    Mockito.verify(kafkaTopicProperties, times(3)).getCategoryToBusinessPartnerMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getCategoryToProductCodeMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(4)).getCategoryToProductSkuMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getProductSkuToSalesCatalogMappingEvent();
  }

  @Test
  public void processExceptionTest() throws Exception {
    BulkProcess bulkProcess = createBulkProcess(STORE_ID, REQUEST_ID, BULK_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), BUSINESS_PARTNER_CODE, "excelName");
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue(STORE_ID, BUSINESS_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), getArgs());
    Map<String, String> files = this.getFile();
    byte[] excelFile = Base64.decodeBase64(files.get("xls"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator +
        bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_EXCEL, excelFile);
    Mockito.when(kafkaTopicProperties.getCategoryToBusinessPartnerMappingEvent()).thenReturn(EVENT);
    Mockito.doThrow(ApplicationRuntimeException.class).when(kafkaProducer)
        .send(Mockito.eq(EVENT), Mockito.any(CategoryUserMappingRequest.class));
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(bulkProcess);
    serviceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE);
    Mockito.verify(bulkProcessRepository, times(2)).save(Mockito.eq(bulkProcess));
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToBusinessPartnerMappingEvent()), Mockito.any(CategoryUserMappingRequest.class));
    Mockito.verify(kafkaProducer, times(3)).send(
        Mockito.eq(kafkaTopicProperties.getCategoryToProductSkuMappingEvent()), Mockito.any(CategoryProductSkuMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito
        .eq(kafkaTopicProperties.getProductSkuToSalesCatalogMappingEvent()), Mockito.any(ProductSkuToSalesCatalogMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToProductCodeMappingEvent()), Mockito.any(BulkDataForRecategorizationRequest.class));
    Mockito.verify(notificationService).sendNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(), false);
    Mockito.verify(kafkaTopicProperties, times(5)).getCategoryToBusinessPartnerMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getCategoryToProductCodeMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(4)).getCategoryToProductSkuMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getProductSkuToSalesCatalogMappingEvent();
  }

  @Test
  public void processExceptionTest2() throws Exception {
    BulkProcess bulkProcess = createBulkProcess(STORE_ID, REQUEST_ID, BULK_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), BUSINESS_PARTNER_CODE, "excelName");
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue(STORE_ID, BUSINESS_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), getArgs());
    Map<String, String> files = this.getFile();
    byte[] excelFile = Base64.decodeBase64(files.get("xls"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator +
        bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_EXCEL, excelFile);
    Mockito.when(kafkaTopicProperties.getCategoryToProductCodeMappingEvent()).thenReturn("event1");
    Mockito.doThrow(ApplicationRuntimeException.class).when(kafkaProducer)
        .send(Mockito.eq("event1"),
            Mockito.any(BulkDataForRecategorizationRequest.class));
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(bulkProcess);
    serviceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE);
    Mockito.verify(bulkProcessRepository, times(2)).save(Mockito.eq(bulkProcess));
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToBusinessPartnerMappingEvent()), Mockito.any(CategoryUserMappingRequest.class));
    Mockito.verify(kafkaProducer, times(3)).send(
        Mockito.eq(kafkaTopicProperties.getCategoryToProductSkuMappingEvent()), Mockito.any(CategoryProductSkuMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito
        .eq(kafkaTopicProperties.getProductSkuToSalesCatalogMappingEvent()), Mockito.any(ProductSkuToSalesCatalogMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToProductCodeMappingEvent()), Mockito.any(BulkDataForRecategorizationRequest.class));
    Mockito.verify(notificationService).sendNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(), false);
    Mockito.verify(kafkaTopicProperties, times(3)).getCategoryToBusinessPartnerMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(3)).getCategoryToProductCodeMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(4)).getCategoryToProductSkuMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getProductSkuToSalesCatalogMappingEvent();
  }

  @Test
  public void processExceptionTest3() throws Exception {
    BulkProcess bulkProcess = createBulkProcess(STORE_ID, REQUEST_ID, BULK_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), BUSINESS_PARTNER_CODE, "excelName");
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue(STORE_ID, BUSINESS_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), getArgs());
    Map<String, String> files = this.getFile();
    byte[] excelFile = Base64.decodeBase64(files.get("xls"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator +
        bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_EXCEL, excelFile);
    Mockito.when(kafkaTopicProperties.getCategoryToProductSkuMappingEvent()).thenReturn(EVENT);
    Mockito.doThrow(ApplicationRuntimeException.class).when(kafkaProducer)
        .send(Mockito.eq(EVENT),
            Mockito.any(CategoryProductSkuMappingRequest.class));
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(bulkProcess);
    serviceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE);
    Mockito.verify(bulkProcessRepository, times(2)).save(Mockito.eq(bulkProcess));
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToBusinessPartnerMappingEvent()), Mockito.any(CategoryUserMappingRequest.class));
    Mockito.verify(kafkaProducer, times(3)).send(
        Mockito.eq(EVENT), Mockito.any(CategoryProductSkuMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito
        .eq(kafkaTopicProperties.getProductSkuToSalesCatalogMappingEvent()), Mockito.any(ProductSkuToSalesCatalogMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToProductCodeMappingEvent()), Mockito.any(BulkDataForRecategorizationRequest.class));
    Mockito.verify(notificationService).sendNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(), false);
    Mockito.verify(kafkaTopicProperties, times(3)).getCategoryToBusinessPartnerMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getCategoryToProductCodeMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(6)).getCategoryToProductSkuMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getProductSkuToSalesCatalogMappingEvent();
  }

  @Test
  public void processExceptionTest4() throws Exception {
    BulkProcess bulkProcess = createBulkProcess(STORE_ID, REQUEST_ID, BULK_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), BUSINESS_PARTNER_CODE, "excelName");
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue(STORE_ID, BUSINESS_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), getArgs());
    Map<String, String> files = this.getFile();
    byte[] excelFile = Base64.decodeBase64(files.get("xls"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator +
        bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_EXCEL, excelFile);
    Mockito.when(kafkaTopicProperties.getProductSkuToSalesCatalogMappingEvent()).thenReturn(EVENT);
    Mockito.doThrow(ApplicationRuntimeException.class).when(kafkaProducer)
        .send(Mockito.eq(EVENT), Mockito.any(ProductSkuToSalesCatalogMappingRequest.class));
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(bulkProcess);
    serviceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE);
    Mockito.verify(bulkProcessRepository, times(2)).save(Mockito.eq(bulkProcess));
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToBusinessPartnerMappingEvent()), Mockito.any(CategoryUserMappingRequest.class));
    Mockito.verify(kafkaProducer, times(3)).send(
        Mockito.eq(kafkaTopicProperties.getCategoryToProductSkuMappingEvent()), Mockito.any(CategoryProductSkuMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito
        .eq(EVENT), Mockito.any(ProductSkuToSalesCatalogMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToProductCodeMappingEvent()), Mockito.any(BulkDataForRecategorizationRequest.class));
    Mockito.verify(notificationService).sendNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(), false);
    Mockito.verify(kafkaTopicProperties, times(3)).getCategoryToBusinessPartnerMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getCategoryToProductCodeMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(4)).getCategoryToProductSkuMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getProductSkuToSalesCatalogMappingEvent();
  }

  @Test
  public void processTest_WhenDomainPublisherThrowError() throws Exception {
    BulkProcess bulkProcess = createBulkProcess(STORE_ID, REQUEST_ID, BULK_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), BUSINESS_PARTNER_CODE, "excelName");
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue(STORE_ID, BUSINESS_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), getArgs());
    Map<String, String> files = this.getFile();
    byte[] excelFile = Base64.decodeBase64(files.get("xls"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator +
        bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_EXCEL, excelFile);

    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(bulkProcess);
    Mockito.doThrow(new NullPointerException()).when(kafkaProducer).send(Mockito.any(), Mockito.any(),
        Mockito.any());
    serviceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE);
    Mockito.verify(bulkProcessRepository, times(2)).save(Mockito.eq(bulkProcess));
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToBusinessPartnerMappingEvent()), Mockito.any(CategoryUserMappingRequest.class));
    Mockito.verify(kafkaProducer, times(3)).send(
        Mockito.eq(kafkaTopicProperties.getCategoryToProductSkuMappingEvent()), Mockito.any(CategoryProductSkuMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito
        .eq(kafkaTopicProperties.getProductSkuToSalesCatalogMappingEvent()), Mockito.any(ProductSkuToSalesCatalogMappingRequest.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getCategoryToProductCodeMappingEvent()), Mockito.any(BulkDataForRecategorizationRequest.class));
    Mockito.verify(notificationService).sendNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(), false);
    Mockito.verify(kafkaTopicProperties, times(3)).getCategoryToBusinessPartnerMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getCategoryToProductCodeMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(4)).getCategoryToProductSkuMappingEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getProductSkuToSalesCatalogMappingEvent();
  }

  @Test
  public void processTest_WhenInvalidFile() throws Exception {
    BulkProcess bulkProcess = createBulkProcess(STORE_ID, REQUEST_ID, BULK_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), BUSINESS_PARTNER_CODE, "excelName");
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue(STORE_ID, BUSINESS_PARTNER_CODE, BulkProcessType
        .RECATEGORIZATION.getValue(), getArgs());
    Map<String, String> files = this.getFileInvalid();
    byte[] excelFile = Base64.decodeBase64(files.get("xls"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator +
        bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_EXCEL, excelFile);
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(bulkProcess);
    serviceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository).findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BUSINESS_PARTNER_CODE);
    Mockito.verify(bulkProcessRepository, times(2)).save(Mockito.eq(bulkProcess));
    Mockito.verify(notificationService).sendNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(), false);
  }

  private BulkProcess createBulkProcess(String storeId, String requestId, String bulkProcessCode,
      String bulkProcessType, String businessPartnerCode, String excelFilename) {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStoreId(storeId);
    bulkProcess.setBulkProcessCode(bulkProcessCode);
    bulkProcess.setBulkUpdate(false);
    bulkProcess.setBulkProcessType(bulkProcessType);
    bulkProcess.setBusinessPartnerCode(businessPartnerCode);
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcess.setRequestId(requestId);
    bulkProcess.setDescription(excelFilename + ". " + ProductLevel3ProcessorServiceBean.DESCRIPTION_PENDING);
    bulkProcess.setErrorCount(0);
    bulkProcess.setSuccessCount(0);
    bulkProcess.setTotalCount(0);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setSystemErrorCount(0);
    bulkProcess.setCreatedBy(GdnMandatoryRequestParameterUtil.getUsername());
    bulkProcess.setUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
    return bulkProcess;
  }

  private Map<String, String> getFile() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread()
        .getContextClassLoader().getResourceAsStream("Recategorization" + File.separator + "recategorization_valid"
            + ".xls"))), "UTF-8");
    files.put("xls", excelData);
    return files;
  }

  private Map<String, String> getFileInvalid() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread()
        .getContextClassLoader().getResourceAsStream("Recategorization" + File.separator + "recategorization_blank"
            + ".xls"))), "UTF-8");
    files.put("xls", excelData);
    return files;
  }

  private Map<String, String> getFileBlank() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    files.put("xls", "");
    return files;
  }

  private Map<String, String> getArgs() throws Exception {
    Map<String, String> args = new HashMap<String, String>();
    args.put("excelFilename", "recategorization_valid.xls");
    return args;
  }
}
