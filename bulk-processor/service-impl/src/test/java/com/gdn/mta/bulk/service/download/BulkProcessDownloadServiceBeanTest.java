package com.gdn.mta.bulk.service.download;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkDownloadEntityStatus;
import com.gdn.mta.bulk.BulkDownloadException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkInternalProcessDTO;
import com.gdn.mta.bulk.dto.BulkPendingRequestsResponse;
import com.gdn.mta.bulk.dto.BulkProcessPath;
import com.gdn.mta.bulk.dto.TaggedProductFilterRequest;
import com.gdn.mta.bulk.dto.product.TaggedProductFilterDTO;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.factory.BulkProcessDataFactory;
import com.gdn.mta.bulk.factory.BulkProcessHelperFactory;
import com.gdn.mta.bulk.helper.BulkInternalFailedProductsProcessHelper;
import com.gdn.mta.bulk.helper.BulkMasterProductProcessHelper;
import com.gdn.mta.bulk.helper.BulkProcessHelper;
import com.gdn.mta.bulk.helper.BulkProductProcessHelper;
import com.gdn.mta.bulk.helper.BulkRecatFailedProductsProcessHelper;
import com.gdn.mta.bulk.helper.BulkStoreCopyProcessHelper;
import com.gdn.mta.bulk.helper.BulkVendorProcessHelper;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.InternalProcessFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.MasterProductDownloadRequest;
import com.gdn.mta.bulk.models.download.MasterSelectedProductDownloadRequest;
import com.gdn.mta.bulk.models.download.OrderDownloadRequest;
import com.gdn.mta.bulk.models.download.ProductDownloadRequest;
import com.gdn.mta.bulk.models.download.ProductVendorDownloadRequest;
import com.gdn.mta.bulk.models.download.RecatFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.FileStorageService;
import com.gdn.mta.bulk.service.MailDeliveryService;
import com.gdn.mta.bulk.service.NotificationService;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;

/**
 * Created by keshashah on 13/11/16.
 */
public class BulkProcessDownloadServiceBeanTest {

  private static final String FILE_NAME = "file-name";
  private static final String STORE_ID = "10001";
  private static final String USERNAME = "USERNAME";
  private static final String BP_CODE = "DEFAULT-BP-CODE";

  @Mock
  private BulkProcessDataFactory bulkProcessDataFactory;

  @Mock
  private BulkProcessHelperFactory bulkProcessHelperFactory;

  @Mock
  private BulkDownloadAuditService bulkDownloadAuditService;

  @Mock
  private MailDeliveryService mailDeliveryService;

  @InjectMocks
  private BulkProcessDownloadServiceBean bulkProcessDownloadService;

  @Mock
  private BulkProductDataServiceBean bulkProductDataServiceBean;

  @Mock
  private BulkPDTDataServiceBean bulkPDTDataServiceBean;

  @Mock
  private BulkProductProcessHelper bulkProductProcessHelper;

  @Mock
  private BulkStoreCopyProcessHelper bulkStoreCopyProcessHelper;

  @Mock
  private BulkMasterProductProcessHelper bulkMasterProductProcessHelper;

  @Mock
  private BulkVendorProcessHelper bulkVendorProcessHelper;

  @Mock
  private BulkProcessFileGeneration bulkProcessFileGeneration;

  @Mock
  private BulkOrderDataServiceBean bulkOrderDataServiceBean;

  @Mock
  private BulkDownloadMasterProductServiceBean bulkDownloadMasterProductServiceBean;

  @Mock
  private BulkStoreCopyProductsServiceBean bulkStoreCopyProductsServiceBean;

  @Mock
  private BulkSelectedMasterProductsDownloadServiceBean bulkSelectedMasterProductsDownloadServiceBean;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private NotificationService notificationService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private BulkProcessService bulkProcessService;

  @Captor
  private ArgumentCaptor<TaggedProductFilterDTO> taggedProductFilterDTOArgumentCaptor;

  private RecatFailedProductsDownloadRequest bulkDownloadRequest;
  private BulkRecatFailedProductsProcessHelper bulkRecatFailedProductsProcessHelper;
  private BulkInternalFailedProductsProcessHelper bulkInternalFailedProductsProcessHelper;
  private InternalProcessFailedProductsDownloadRequest internalProcessFailedProductsDownloadRequest;
  private ProfileResponse profileResponse;

  @BeforeEach
  public void setUp() throws Exception {
    bulkDownloadRequest = new RecatFailedProductsDownloadRequest();
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.RECAT_FAILED_PRODUCTS);
    bulkDownloadRequest.setFilename(FILE_NAME);
    bulkDownloadRequest.setMerchantId(BP_CODE);
    bulkDownloadRequest.setUsername(USERNAME);
    initMocks(this);
    bulkRecatFailedProductsProcessHelper = new BulkRecatFailedProductsProcessHelper();

    //bulkInternalProcess
    internalProcessFailedProductsDownloadRequest = new InternalProcessFailedProductsDownloadRequest();
    internalProcessFailedProductsDownloadRequest.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS);
    internalProcessFailedProductsDownloadRequest.setFilename(FILE_NAME);
    bulkInternalFailedProductsProcessHelper = new BulkInternalFailedProductsProcessHelper();
    BulkDownloadEntity bulkDownloadEntity = new BulkDownloadEntity();
    bulkDownloadEntity.setRequestBody("Request");
    bulkDownloadEntity.setId((long) 12);
    bulkDownloadEntity.setBusinessPartnerCode(BP_CODE);
    when(bulkDownloadAuditService.getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2))
        .thenReturn(Arrays.asList(bulkDownloadEntity));
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenReturn(bulkDownloadRequest);
    when(bulkDownloadAuditService.saveBulkDownloadEntity(any())).thenReturn(bulkDownloadEntity);
    when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE, "2", ""));

    profileResponse = new ProfileResponse();

    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
      Mockito.anyString())).thenReturn(profileResponse);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(bulkProcessDataFactory);
    verifyNoMoreInteractions(bulkProcessHelperFactory);
    verifyNoMoreInteractions(bulkDownloadAuditService);
    verifyNoMoreInteractions(mailDeliveryService);
    verifyNoMoreInteractions(bulkProductDataServiceBean);
    verifyNoMoreInteractions(bulkOrderDataServiceBean);
    verifyNoMoreInteractions(bulkProcessFileGeneration);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(bulkPDTDataServiceBean);
    verifyNoMoreInteractions(bulkDownloadMasterProductServiceBean);
    verifyNoMoreInteractions(bulkSelectedMasterProductsDownloadServiceBean);
    verifyNoMoreInteractions(systemParameterConfigService);
    verifyNoMoreInteractions(notificationService);
  }

  @Test
  public void processDownload_product() throws Exception {
    ProductDownloadRequest.ProductBuilder productBuilder = new ProductDownloadRequest.ProductBuilder();
    productBuilder.privilegedMap(new HashMap<String, Boolean>());
    productBuilder.productSize(1);
    productBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    productBuilder.emailCC("kesha@xyz.com");
    productBuilder.emailTo("kesha@xyz.com");
    productBuilder.filename("test.xlsx");
    productBuilder.downloadType(DownloadType.ALL);
    productBuilder.merchant("m-123");
    productBuilder.request("123");
    BulkDownloadRequest request = productBuilder.build();
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenReturn(bulkProductDataServiceBean);
    when(bulkProductDataServiceBean.getData(any(BulkDownloadRequest.class))).thenReturn(new BulkDataResponse());
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class))).thenReturn(bulkProductProcessHelper);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkProductDataServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileFromResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class));
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(bulkDownloadAuditService).updateAuditLog(any(), anyString(), anyInt(), any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(true),
        Mockito.eq(false));
  }

  @Test
  public void processDownload_master_product() throws Exception {
    MasterProductDownloadRequest.MasterProductDownloadBuilder productBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    productBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    productBuilder.emailCC("kesha@xyz.com");
    productBuilder.emailTo("kesha@xyz.com");
    productBuilder.filename("test.xlsx");
    productBuilder.downloadType(DownloadType.ALL);
    productBuilder.merchant("m-123");
    productBuilder.request("123");
    BulkDownloadRequest request = productBuilder.build();
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class)))
        .thenReturn(bulkDownloadMasterProductServiceBean);
    when(bulkDownloadMasterProductServiceBean.getData(any(BulkDownloadRequest.class)))
        .thenReturn(new BulkDataResponse());
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class))).thenReturn(bulkMasterProductProcessHelper);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkDownloadMasterProductServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileFromResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class));
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(bulkDownloadAuditService).updateAuditLog(any(), anyString(), anyInt(), any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(true),
        Mockito.eq(false));
  }

  @Test
  public void processDownload_selected_master_product() throws Exception {
    MasterSelectedProductDownloadRequest.MasterSelectedProductBuilder builder =
        new MasterSelectedProductDownloadRequest.MasterSelectedProductBuilder();
    builder.bulkProcessType(BulkProcessEntity.PRODUCT);
    builder.emailCC("kesha@xyz.com");
    builder.emailTo("kesha@xyz.com");
    builder.filename("test.xlsx");
    builder.downloadType(DownloadType.ALL);
    builder.merchant("m-123");
    builder.request("123");
    BulkDownloadRequest request = builder.build();
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    bulkDataResponse.setBusinessPartnerCode(BP_CODE);
    when(bulkProcessDataFactory.getRepository(any(RecatFailedProductsDownloadRequest.class)))
        .thenReturn(bulkSelectedMasterProductsDownloadServiceBean);
    when(bulkSelectedMasterProductsDownloadServiceBean.getData(any(BulkDownloadRequest.class)))
        .thenReturn(bulkDataResponse);
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class))).thenReturn(bulkMasterProductProcessHelper);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkSelectedMasterProductsDownloadServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileFromResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class));
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(bulkDownloadAuditService).updateAuditLog(any(), anyString(), anyInt(), any());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(true),
        Mockito.eq(false));
  }

  @Test
  public void processDownload_product_vendor() throws Exception {
    ProductVendorDownloadRequest.ProductVendorDownloadBuilder productVendorBuilder =
        new ProductVendorDownloadRequest.ProductVendorDownloadBuilder();
    productVendorBuilder.productSize(1);
    productVendorBuilder.bulkProcessType(BulkProcessEntity.PRODUCT_VENDOR);
    productVendorBuilder.filename("test.xlsx");
    productVendorBuilder.downloadType(DownloadType.ALL);
    productVendorBuilder.request("123");
    productVendorBuilder.vendorCode("vendor-code");
    productVendorBuilder.directDownload(true);
    productVendorBuilder.productListRequest(new ProductListRequest());
    BulkDownloadRequest request = productVendorBuilder.build();
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenReturn(request);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenReturn(bulkPDTDataServiceBean);
    when(bulkPDTDataServiceBean.getData(any(BulkDownloadRequest.class))).thenReturn(new BulkDataResponse());
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class))).thenReturn(bulkVendorProcessHelper);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkPDTDataServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileFromResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class));
    verify(bulkDownloadAuditService).updateAuditLog(anyString(), anyString(), anyInt(), anyString());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(true),
        Mockito.eq(false));
  }

  @Test
  public void processDownload_product_exception() throws Exception {
    ProductDownloadRequest.ProductBuilder productBuilder = new ProductDownloadRequest.ProductBuilder();
    productBuilder.privilegedMap(new HashMap<String, Boolean>());
    productBuilder.productSize(1);
    productBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    productBuilder.emailCC("kesha@xyz.com");
    productBuilder.emailTo("kesha@xyz.com");
    productBuilder.filename("test.xlsx");
    productBuilder.downloadType(DownloadType.ALL);
    productBuilder.merchant("m-123");
    productBuilder.request("123");
    BulkDownloadRequest request = productBuilder.build();
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenThrow(ApplicationRuntimeException.class);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(mailDeliveryService).sendBulkDownloadErrorMail(any(BulkDownloadRequest.class));
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).updateAuditLog(any(), anyString(), anyInt(), any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(false),
        Mockito.eq(false));
  }

  @Test
  public void processDownload_master_product_exception() throws Exception {
    MasterProductDownloadRequest.MasterProductDownloadBuilder productBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    productBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    productBuilder.emailCC("kesha@xyz.com");
    productBuilder.emailTo("kesha@xyz.com");
    productBuilder.filename("test.xlsx");
    productBuilder.downloadType(DownloadType.ALL);
    productBuilder.merchant("m-123");
    productBuilder.request("123");
    BulkDownloadRequest request = productBuilder.build();
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenThrow(ApplicationRuntimeException.class);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(mailDeliveryService).sendBulkDownloadErrorMail(any(BulkDownloadRequest.class));
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).updateAuditLog(any(), anyString(), anyInt(), any());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(false),
        Mockito.eq(false));
  }

  @Test
  public void processDownloadselected_master_product_exception() throws Exception {
    MasterSelectedProductDownloadRequest.MasterSelectedProductBuilder productBuilder =
        new MasterSelectedProductDownloadRequest.MasterSelectedProductBuilder();
    productBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    productBuilder.emailCC("kesha@xyz.com");
    productBuilder.emailTo("kesha@xyz.com");
    productBuilder.filename("test.xlsx");
    productBuilder.downloadType(DownloadType.ALL);
    productBuilder.merchant("m-123");
    productBuilder.request("123");
    BulkDownloadRequest request = productBuilder.build();
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenThrow(ApplicationRuntimeException.class);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(mailDeliveryService).sendBulkDownloadErrorMail(any(BulkDownloadRequest.class));
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).updateAuditLog(any(), anyString(), anyInt(), any());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(false),
        Mockito.eq(false));
  }

  @Test
  public void processDownload_order() throws Exception {
    OrderDownloadRequest.OrderBuilder orderBuilder = new OrderDownloadRequest.OrderBuilder();
    orderBuilder.orderRequest(new OrderItemSummaryRequest());
    orderBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    orderBuilder.emailCC("kesha@xyz.com");
    orderBuilder.emailTo("kesha@xyz.com");
    orderBuilder.filename("test.xlsx");
    orderBuilder.downloadType(DownloadType.ALL);
    orderBuilder.merchant("m-123");
    orderBuilder.request("123");
    BulkDownloadRequest request = orderBuilder.build();
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenReturn(bulkOrderDataServiceBean);
    when(bulkOrderDataServiceBean.getData(any(BulkDownloadRequest.class))).thenReturn(new BulkDataResponse());
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class))).thenReturn(bulkProductProcessHelper);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkOrderDataServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileFromResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class));
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(bulkDownloadAuditService).updateAuditLog(any(), anyString(), anyInt(), any());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(true),
        Mockito.eq(false));
  }

  @Test
  public void processDownload_exception_occurred_sendFailureMail() throws Exception {
    OrderDownloadRequest.OrderBuilder orderBuilder = new OrderDownloadRequest.OrderBuilder();
    orderBuilder.orderRequest(new OrderItemSummaryRequest());
    orderBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    orderBuilder.emailCC("kesha@xyz.com");
    orderBuilder.emailTo("kesha@xyz.com");
    orderBuilder.filename("test.xlsx");
    orderBuilder.downloadType(DownloadType.ALL);
    orderBuilder.merchant("m-123");
    orderBuilder.request("123");
    BulkDownloadRequest request = orderBuilder.build();
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenReturn(bulkOrderDataServiceBean);
    when(bulkOrderDataServiceBean.getData(any(BulkDownloadRequest.class)))
        .thenThrow(new BulkDownloadException("some code", "some message"));
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkOrderDataServiceBean).getData(any(BulkDownloadRequest.class));
    verify(mailDeliveryService).sendBulkDownloadErrorMail(any(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).updateAuditLog(any(), anyString(), anyInt(), any());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(false),
        Mockito.eq(false));
  }

  @Test
  public void processDownload_product_directDownload() throws Exception {
    profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(true);
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Mockito.anyString(),
      Mockito.anyString())).thenReturn(profileResponse);
    ProductDownloadRequest.ProductBuilder productBuilder = new ProductDownloadRequest.ProductBuilder();
    productBuilder.privilegedMap(new HashMap<String, Boolean>());
    productBuilder.productSize(1);
    productBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    productBuilder.emailCC("kesha@xyz.com");
    productBuilder.emailTo("kesha@xyz.com");
    productBuilder.filename("test.xlsx");
    productBuilder.downloadType(DownloadType.ALL);
    productBuilder.merchant("m-123");
    productBuilder.request("123");
    productBuilder.directDownload(true);
    BulkDownloadRequest request = productBuilder.build();
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenReturn(request);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenReturn(bulkProductDataServiceBean);
    when(bulkProductDataServiceBean.getData(any(BulkDownloadRequest.class))).thenReturn(new BulkDataResponse());
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class))).thenReturn(bulkProductProcessHelper);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkProductDataServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileFromResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class));
    verify(bulkDownloadAuditService).updateAuditLog(anyString(), anyString(), anyInt(), anyString());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(true),
        Mockito.eq(false));
  }

  @Test
  public void processDownload_exception_occurred_exception_directDownload() throws Exception {
    OrderDownloadRequest.OrderBuilder orderBuilder = new OrderDownloadRequest.OrderBuilder();
    orderBuilder.orderRequest(new OrderItemSummaryRequest());
    orderBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    orderBuilder.emailCC("kesha@xyz.com");
    orderBuilder.emailTo("kesha@xyz.com");
    orderBuilder.filename("test.xlsx");
    orderBuilder.downloadType(DownloadType.ALL);
    orderBuilder.merchant("m-123");
    orderBuilder.request("123");
    orderBuilder.directDownload(true);
    BulkDownloadRequest request = orderBuilder.build();
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenReturn(request);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenReturn(bulkOrderDataServiceBean);
    when(bulkOrderDataServiceBean.getData(any(BulkDownloadRequest.class)))
        .thenThrow(new BulkDownloadException("some code", "some message"));
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkOrderDataServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).updateAuditLog(anyString(), anyString(), anyInt(), anyString());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(false),
        Mockito.eq(false));
  }

  @Test
  public void processDownload_master_products_exception_occurred_exception_directDownload() throws Exception {
    MasterProductDownloadRequest.MasterProductDownloadBuilder masterProductDownloadBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    masterProductDownloadBuilder.bulkProcessType(BulkProcessEntity.MASTER_PRODUCT);
    masterProductDownloadBuilder.emailCC("kesha@xyz.com");
    masterProductDownloadBuilder.emailTo("kesha@xyz.com");
    masterProductDownloadBuilder.filename("test.xlsx");
    masterProductDownloadBuilder.downloadType(DownloadType.ALL);
    masterProductDownloadBuilder.merchant("m-123");
    masterProductDownloadBuilder.request("123");
    masterProductDownloadBuilder.directDownload(true);
    BulkDownloadRequest request = masterProductDownloadBuilder.build();
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenReturn(request);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class)))
        .thenReturn(bulkDownloadMasterProductServiceBean);
    when(bulkDownloadMasterProductServiceBean.getData(any(BulkDownloadRequest.class)))
        .thenThrow(new BulkDownloadException("some code", "some message"));
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkDownloadMasterProductServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).updateAuditLog(anyString(), anyString(), anyInt(), anyString());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
  }

  @Test
  public void processDownload_master_product_exception_occurred_sendFailureMail() throws Exception {
    MasterProductDownloadRequest.MasterProductDownloadBuilder masterProductDownloadBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    masterProductDownloadBuilder.bulkProcessType(BulkProcessEntity.MASTER_PRODUCT);
    masterProductDownloadBuilder.emailCC("kesha@xyz.com");
    masterProductDownloadBuilder.emailTo("kesha@xyz.com");
    masterProductDownloadBuilder.filename("test.xlsx");
    masterProductDownloadBuilder.downloadType(DownloadType.ALL);
    masterProductDownloadBuilder.merchant("m-123");
    masterProductDownloadBuilder.request("123");
    BulkDownloadRequest request = masterProductDownloadBuilder.build();
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenReturn(request);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class)))
        .thenReturn(bulkDownloadMasterProductServiceBean);
    when(bulkDownloadMasterProductServiceBean.getData(any(BulkDownloadRequest.class)))
        .thenThrow(new BulkDownloadException("some code", "some message"));
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkDownloadMasterProductServiceBean).getData(any(BulkDownloadRequest.class));
    verify(mailDeliveryService).sendBulkDownloadErrorMail(any(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).updateAuditLog(anyString(), anyString(), anyInt(), anyString());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
  }

  @Test
  public void processDownload_master_product_directDownload() throws Exception {
    MasterProductDownloadRequest.MasterProductDownloadBuilder masterProductDownloadBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    masterProductDownloadBuilder.filterName("apple");
    masterProductDownloadBuilder.bulkProcessType(BulkProcessEntity.MASTER_PRODUCT);
    masterProductDownloadBuilder.emailCC("kesha@xyz.com");
    masterProductDownloadBuilder.emailTo("kesha@xyz.com");
    masterProductDownloadBuilder.filename("test.xlsx");
    masterProductDownloadBuilder.downloadType(DownloadType.ALL);
    masterProductDownloadBuilder.merchant("m-123");
    masterProductDownloadBuilder.request("123");
    masterProductDownloadBuilder.directDownload(true);
    BulkDownloadRequest request = masterProductDownloadBuilder.build();
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenReturn(request);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class)))
        .thenReturn(bulkDownloadMasterProductServiceBean);
    when(bulkDownloadMasterProductServiceBean.getData(any(BulkDownloadRequest.class)))
        .thenReturn(new BulkDataResponse());
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class))).thenReturn(bulkProductProcessHelper);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkDownloadMasterProductServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileFromResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class));
    verify(bulkDownloadAuditService).updateAuditLog(anyString(), anyString(), anyInt(), anyString());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
  }

  @Test
  public void processDownload_copyStore() throws Exception {
    MasterProductDownloadRequest.MasterProductDownloadBuilder masterProductDownloadBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    masterProductDownloadBuilder.filterName("apple");
    masterProductDownloadBuilder.bulkProcessType(BulkProcessEntity.STORE_COPY_PRODUCTS);
    masterProductDownloadBuilder.emailCC("kesha@xyz.com");
    masterProductDownloadBuilder.emailTo("kesha@xyz.com");
    masterProductDownloadBuilder.filename("test.xlsx");
    masterProductDownloadBuilder.downloadType(DownloadType.ALL);
    masterProductDownloadBuilder.merchant("m-123");
    masterProductDownloadBuilder.request("123");
    masterProductDownloadBuilder.directDownload(true);
    BulkDownloadRequest request = masterProductDownloadBuilder.build();
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenReturn(request);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class)))
        .thenReturn(bulkStoreCopyProductsServiceBean);
    when(bulkStoreCopyProductsServiceBean.getData(any(BulkDownloadRequest.class))).thenReturn(new BulkDataResponse());
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class))).thenReturn(bulkStoreCopyProcessHelper);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkStoreCopyProductsServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileFromResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class));
    verify(bulkDownloadAuditService).updateAuditLog(anyString(), anyString(), anyInt(), anyString());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
  }

  @Test
  public void processDownload_copyStoreNewFlow() throws Exception {
    ReflectionTestUtils.setField(bulkProcessDownloadService,  "processDownloadNewFlow", true);
    MasterProductDownloadRequest.MasterProductDownloadBuilder masterProductDownloadBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    masterProductDownloadBuilder.filterName("apple");
    masterProductDownloadBuilder.bulkProcessType(BulkProcessEntity.STORE_COPY_PRODUCTS);
    masterProductDownloadBuilder.emailCC("kesha@xyz.com");
    masterProductDownloadBuilder.emailTo("kesha@xyz.com");
    masterProductDownloadBuilder.filename("test.xlsx");
    masterProductDownloadBuilder.downloadType(DownloadType.ALL);
    masterProductDownloadBuilder.merchant("m-123");
    masterProductDownloadBuilder.request("123");
    masterProductDownloadBuilder.directDownload(true);
    BulkDownloadRequest request = masterProductDownloadBuilder.build();
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenReturn(request);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class)))
        .thenReturn(bulkStoreCopyProductsServiceBean);
    when(bulkStoreCopyProductsServiceBean.getData(any(BulkDownloadRequest.class))).thenReturn(new BulkDataResponse());
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class))).thenReturn(bulkStoreCopyProcessHelper);
    BulkDownloadEntity bulkDownloadEntity = new BulkDownloadEntity();
    bulkDownloadEntity.setRequestBody("Request");
    bulkDownloadEntity.setId((long) 12);
    bulkDownloadEntity.setBusinessPartnerCode(BP_CODE);
    when(bulkDownloadAuditService.getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2))
        .thenReturn(List.of(bulkDownloadEntity));
    when(bulkDownloadAuditService.saveBulkDownloadEntityList(Arrays.asList(bulkDownloadEntity))).thenReturn(
        List.of(bulkDownloadEntity));
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkStoreCopyProductsServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileFromResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class));
    verify(bulkDownloadAuditService).updateAuditLog(anyString(), anyString(), anyInt(), anyString());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntityList(List.of(bulkDownloadEntity));
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
  }

  @Test
  public void processDownload_copy_products_exception_occurred_sendFailureMail() throws Exception {
    MasterProductDownloadRequest.MasterProductDownloadBuilder masterProductDownloadBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    masterProductDownloadBuilder.bulkProcessType(BulkProcessEntity.STORE_COPY_PRODUCTS);
    masterProductDownloadBuilder.emailCC("kesha@xyz.com");
    masterProductDownloadBuilder.emailTo("kesha@xyz.com");
    masterProductDownloadBuilder.filename("test.xlsx");
    masterProductDownloadBuilder.downloadType(DownloadType.ALL);
    masterProductDownloadBuilder.merchant("m-123");
    masterProductDownloadBuilder.request("123");
    BulkDownloadRequest request = masterProductDownloadBuilder.build();
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenReturn(request);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class)))
        .thenReturn(bulkStoreCopyProductsServiceBean);
    when(bulkStoreCopyProductsServiceBean.getData(any(BulkDownloadRequest.class)))
        .thenThrow(new BulkDownloadException("some code", "some message"));
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkStoreCopyProductsServiceBean).getData(any(BulkDownloadRequest.class));
    verify(mailDeliveryService).sendBulkDownloadErrorMail(any(BulkDownloadRequest.class));
    verify(bulkDownloadAuditService).updateAuditLog(anyString(), anyString(), anyInt(), anyString());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
  }

  @Test
  public void processDownload_campaign_product_directDownload() throws Exception {
    MasterProductDownloadRequest.MasterProductDownloadBuilder masterProductDownloadBuilder =
        new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    masterProductDownloadBuilder.filterName("apple");
    masterProductDownloadBuilder.bulkProcessType(BulkProcessEntity.CAMPAIGN_PRODUCT);
    masterProductDownloadBuilder.emailCC("kesha@xyz.com");
    masterProductDownloadBuilder.emailTo("kesha@xyz.com");
    masterProductDownloadBuilder.filename("test.xlsx");
    masterProductDownloadBuilder.downloadType(DownloadType.ALL);
    masterProductDownloadBuilder.merchant("m-123");
    masterProductDownloadBuilder.request("123");
    masterProductDownloadBuilder.directDownload(true);
    BulkDownloadRequest request = masterProductDownloadBuilder.build();
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenReturn(request);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class)))
        .thenReturn(bulkDownloadMasterProductServiceBean);
    when(bulkDownloadMasterProductServiceBean.getData(any(BulkDownloadRequest.class)))
        .thenReturn(new BulkDataResponse());
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class))).thenReturn(bulkProductProcessHelper);
    bulkProcessDownloadService.processDownload(STORE_ID);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkDownloadMasterProductServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileFromResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class));
    verify(bulkDownloadAuditService).updateAuditLog(anyString(), anyString(), anyInt(), anyString());
    verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
    verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    verify(notificationService).sendDownloadNotification(Mockito.any(BulkDownloadRequest.class), Mockito.eq(true),
        Mockito.eq(false));
  }

  @Test
  public void downloadExcelFileTest() throws Exception {
    RecatFailedProductsDownloadRequest recatFailedProductsDownloadRequest =
        (RecatFailedProductsDownloadRequest) bulkDownloadRequest;
    recatFailedProductsDownloadRequest.setRecatRequestCode(STORE_ID);
    BulkInternalProcessDTO bulkInternalProcessDTO = new BulkInternalProcessDTO();
    when(fileStorageService.checkIfFileExistsByFilePath(Mockito.anyString())).thenReturn(Boolean.FALSE);
    when(fileStorageService.getBasePath(Mockito.anyString())).thenReturn(FILE_NAME);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenReturn(bulkProductDataServiceBean);
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class)))
        .thenReturn(bulkRecatFailedProductsProcessHelper);
    bulkInternalProcessDTO.setFilepath("filePath");
    when(bulkProcessFileGeneration
        .generateFileResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class))).thenReturn(bulkInternalProcessDTO);
    bulkProcessDownloadService.downloadExcelFile(recatFailedProductsDownloadRequest, BulkProcessPath.RECAT.getValue());
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProductDataServiceBean).getData(any(BulkDownloadRequest.class));
    verify(fileStorageService).getBasePath(Mockito.anyString());
    verify(fileStorageService).checkIfFileExistsByFilePath(Mockito.anyString());
    verify(bulkProcessFileGeneration)
        .generateFileResponse(Mockito.any(),
            Mockito.any(), Mockito.any());
  }

  @Test
  public void downloadExcelFileExistTest() throws Exception {
    RecatFailedProductsDownloadRequest recatFailedProductsDownloadRequest =
        (RecatFailedProductsDownloadRequest) bulkDownloadRequest;
    recatFailedProductsDownloadRequest.setRecatRequestCode(STORE_ID);
    when(fileStorageService.checkIfFileExistsByFilePath(Mockito.anyString())).thenReturn(Boolean.TRUE);
    when(fileStorageService.getBasePath(Mockito.anyString())).thenReturn(FILE_NAME);
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class)))
        .thenReturn(bulkRecatFailedProductsProcessHelper);
    String filePath = bulkRecatFailedProductsProcessHelper
        .getFilePath(bulkRecatFailedProductsProcessHelper.getDirectory(bulkDownloadRequest),
            bulkDownloadRequest.getFilename());
    File file = new File(filePath);
    file.mkdirs();
    bulkProcessDownloadService.downloadExcelFile(recatFailedProductsDownloadRequest, BulkProcessPath.RECAT.getValue());
    verify(fileStorageService).getBasePath(Mockito.anyString());
    verify(fileStorageService).checkIfFileExistsByFilePath(Mockito.anyString());
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    file.delete();
  }

  @Test
  public void internalProcessFailedDownloadExcelFileTest() throws Exception {
    BulkInternalProcessDTO bulkInternalProcessDTO = new BulkInternalProcessDTO();
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class)))
        .thenReturn(bulkInternalFailedProductsProcessHelper);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenReturn(bulkProductDataServiceBean);
    when(fileStorageService.createBulkInternalFile(Mockito.any(),
        Mockito.any(), Mockito.any())).thenReturn(STORE_ID);
    String filePath = bulkInternalFailedProductsProcessHelper
        .getFilePath(bulkInternalFailedProductsProcessHelper.getDirectory(internalProcessFailedProductsDownloadRequest),
            internalProcessFailedProductsDownloadRequest.getFilename());
    bulkInternalProcessDTO.setFilepath(filePath);
    when(bulkProcessFileGeneration
        .generateFileResponse(any(BulkDownloadRequest.class), any(BulkDataResponse.class),
            any(BulkProcessHelper.class))).thenReturn(bulkInternalProcessDTO);
    bulkProcessDownloadService.internalProcessFailedDownloadExcelFile(internalProcessFailedProductsDownloadRequest,
        "internal");
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProductDataServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileResponse(Mockito.any(),
            Mockito.any(), Mockito.any());
    verify(fileStorageService).createBulkInternalFile(any(BulkDownloadRequest.class), Mockito.any(), Mockito.anyString());
    Assertions.assertEquals("target/x-bulk/storeCopy/null/file-name", filePath);
  }

  @Test
  public void downloadAndOverwriteExcelFileTest() throws Exception {
    when(bulkProcessFileGeneration.generateFileFromResponse(Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(
        STORE_ID);
    when(bulkProcessHelperFactory.getHelper(any(BulkDownloadRequest.class)))
        .thenReturn(bulkInternalFailedProductsProcessHelper);
    when(bulkProcessDataFactory.getRepository(any(BulkDownloadRequest.class))).thenReturn(bulkProductDataServiceBean);
    String filePath = bulkInternalFailedProductsProcessHelper
        .getFilePath(bulkInternalFailedProductsProcessHelper.getDirectory(internalProcessFailedProductsDownloadRequest),
            internalProcessFailedProductsDownloadRequest.getFilename());
    bulkProcessDownloadService.downloadAndOverwriteExcelFile(internalProcessFailedProductsDownloadRequest);
    verify(bulkProcessDataFactory).getRepository(any(BulkDownloadRequest.class));
    verify(bulkProcessHelperFactory).getHelper(any(BulkDownloadRequest.class));
    verify(bulkProductDataServiceBean).getData(any(BulkDownloadRequest.class));
    verify(bulkProcessFileGeneration)
        .generateFileFromResponse(any(BulkDownloadRequest.class), Mockito.isNull(),
            any(BulkProcessHelper.class));
    Assertions.assertEquals("target/x-bulk/storeCopy/null/file-name", filePath);
  }

  @Test
  public void downloadAll() throws Exception {
    Mockito.when(
        bulkProcessService.checkForPendingBulkProcess(STORE_ID, USERNAME, Constant.BULK_DOWNLOAD_TYPE, BP_CODE, null,
            BulkProcessEntity.RECAT_FAILED_PRODUCTS.name())).thenReturn(new BulkPendingRequestsResponse(false, 0));
    bulkProcessDownloadService.downloadAll(bulkDownloadRequest);
    verify(bulkDownloadAuditService).createAuditLog(any(BulkDownloadRequest.class), anyString());
    Mockito.verify(bulkProcessService)
        .checkForPendingBulkProcess(STORE_ID, USERNAME, Constant.BULK_DOWNLOAD_TYPE, BP_CODE, null,
            BulkProcessEntity.RECAT_FAILED_PRODUCTS.name());
  }

  @Test
  public void downloadAllSkipped() throws Exception {
    Mockito.when(
        bulkProcessService.checkForPendingBulkProcess(STORE_ID, USERNAME, Constant.BULK_DOWNLOAD_TYPE, BP_CODE, null,
            BulkProcessEntity.RECAT_FAILED_PRODUCTS.name())).thenReturn(new BulkPendingRequestsResponse(false, 1));
    bulkProcessDownloadService.downloadAll(bulkDownloadRequest);
    Mockito.verify(bulkProcessService)
        .checkForPendingBulkProcess(STORE_ID, USERNAME, Constant.BULK_DOWNLOAD_TYPE, BP_CODE, null,
            BulkProcessEntity.RECAT_FAILED_PRODUCTS.name());
  }

  @Test
  public void processDownloadExceptionTest() throws Exception {
    Exception exception = new Exception();
    ProductDownloadRequest.ProductBuilder productBuilder = new ProductDownloadRequest.ProductBuilder();
    productBuilder.privilegedMap(new HashMap<String, Boolean>());
    productBuilder.productSize(1);
    productBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    productBuilder.emailCC("kesha@xyz.com");
    productBuilder.emailTo("kesha@xyz.com");
    productBuilder.filename("test.xlsx");
    productBuilder.downloadType(DownloadType.ALL);
    productBuilder.merchant("m-123");
    productBuilder.request("123");
    when(objectMapper.readValue(anyString(), eq(BulkDownloadRequest.class))).thenThrow(JsonProcessingException.class);
    try{
      bulkProcessDownloadService.processDownload(STORE_ID);
    }
    catch (Exception ex){
      exception = ex;
    }
    finally {
      verify(objectMapper).readValue(anyString(), eq(BulkDownloadRequest.class));
      verify(bulkDownloadAuditService).getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
      verify(bulkDownloadAuditService).saveBulkDownloadEntity(any());
      verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE);
    }
  }

  @Test
  public void processDownloadTaggedProductsTest() throws Exception {
    TaggedProductFilterRequest taggedProductFilterRequest = new TaggedProductFilterRequest();
    taggedProductFilterRequest.setEmailAddress("bulk@gmail.com");
    bulkProcessDownloadService.processDownloadTaggedProducts(taggedProductFilterRequest);
    Mockito.verify(bulkDownloadAuditService).createAuditLog(taggedProductFilterDTOArgumentCaptor.capture(),
        eq(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue()));
    Assertions.assertEquals("bulk@gmail.com", taggedProductFilterDTOArgumentCaptor.getValue().getEmailAddress());
  }
}
