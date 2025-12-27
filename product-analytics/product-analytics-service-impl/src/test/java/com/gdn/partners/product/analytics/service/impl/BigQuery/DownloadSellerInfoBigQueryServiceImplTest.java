package com.gdn.partners.product.analytics.service.impl.BigQuery;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import com.gdn.partners.product.analytics.repository.AutoApprovedRepository;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcess;
import com.gdn.partners.product.analytics.model.BigQueryRequestInfo;
import com.gdn.partners.product.analytics.model.SubmitBigQueryProcessSteps;
import com.gdn.partners.product.analytics.model.enums.FileExtensions;
import com.gdn.partners.product.analytics.model.enums.JobProcessTypes;
import com.gdn.partners.product.analytics.model.enums.ProductAnalyticsProcessStatus;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.service.KafkaProducerService;
import com.gdn.partners.product.analytics.service.UpdateDataFromBigQueryToDB;
import com.gdn.partners.product.analytics.service.bigQuery.ProductAnalyticsProcessService;
import com.gdn.partners.product.analytics.service.helper.BigQueryHelper;
import com.gdn.partners.product.analytics.service.helper.CacheServiceHelper;
import com.gdn.partners.product.analytics.service.helper.FileHelper;
import com.gdn.partners.product.analytics.service.impl.exception.ClientException;
import com.gdn.partners.product.analytics.service.impl.helper.QueryTemplateHelper;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import org.springframework.test.util.ReflectionTestUtils;

public class DownloadSellerInfoBigQueryServiceImplTest {

  private static final String STORE_ID = "10000";
  @Mock
  private GCPProperties gcpProperties;

  @Mock
  private QueryTemplateHelper queryTemplateHelper;

  @Mock
  private BigQueryHelper bigQueryHelper;

  @Mock
  private FileHelper fileHelper;

  @Mock
  private ProductAnalyticsProcessService submitBigQueryProcessHelper;

  @Mock
  private CacheServiceHelper cacheServiceHelper;

  @Mock
  private UpdateDataFromBigQueryToDB updateDataFromBigQueryToDB;

  @Mock
  @Qualifier("SELLER_SPECIFIC_INFO_BQ_JOB")
  private UpdateDataFromBigQueryToDB dataFromBigQueryToSellerSpecificDBImpl;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private KafkaProducerService kafkaProducerService;

  @Mock
  @Qualifier("jedisConnectionFactory")
  private RedisConnectionFactory jedisConnectionFactory;

  @Mock
  private AutoApprovedRepository autoApprovedRepository;

  @InjectMocks
  private DownloadSellerInfoBigQueryServiceImpl submitBigQueryService;

  @Captor
  private ArgumentCaptor<ProductAnalyticsProcess> productAnalyticsProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<BigQueryRequestInfo> bigQueryRequestInfoArgumentCaptor;

  private ProductAnalyticsProcess productAnalyticsProcess;

  private static final String QUERY = "SELECT * FROM somewhere";
  private static final String RESULT_DATA_SET = "resultDataSet";
  private static final String RESULT_TABLE_PREFIX = "resultTablePrefix";
  private static final String SELLER_SPECIFIC_PREFIX = "sellerSpecific";
  private static final String PROJECT_ID = "PROJECT_ID";
  private static final String RESULT_GOOGLE_CLOUD_BUCKET_NAME = "resultGoogleCloudBucketName";
  private static final String RESULT_FILE_NAME_PREFIX = "resultFileNamePrefix";
  private static final String RESULT_FILE_LOCAL = "result";
  private static final int BULK_WRITE_BATCH_SIZE = 50000;
  private static final int NUMBER_OF_RECORDS_PER_FILE = 200000;
  private static final String PROCESS_ID = "processId";
  private static final String SPLIT_FILE_1 = "splitFile1";
  private static final String SPLIT_FILE_2 = "splitFile2";
  private static final String FILE_EXTENSION = FileExtensions.NDJSON.getExtension();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    mockSubmitBigQueryProcessHelper();
    mockGCPProperties();
    mockBigQueryCommandHelper();
    mockCacheManager();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(queryTemplateHelper);
    verifyNoMoreInteractions(gcpProperties);
    verifyNoMoreInteractions(submitBigQueryProcessHelper);
    verifyNoMoreInteractions(cacheServiceHelper);
    verifyNoMoreInteractions(updateDataFromBigQueryToDB);
    verifyNoMoreInteractions(fileHelper);
    verifyNoMoreInteractions(kafkaProducerService);
    verifyNoMoreInteractions(autowireCapableBeanFactory);
    verifyNoMoreInteractions(autoApprovedRepository);
  }

  @Test
  public void execute() throws Exception {
    when(queryTemplateHelper.getFullFetchQuery(JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 0, 24, 0,
        StringUtils.EMPTY, 0)).thenReturn(QUERY);
    when(gcpProperties.isDownloadFileManually()).thenReturn(false);
    when(autowireCapableBeanFactory.getBean(JobProcessTypes.SELLER_INFO_BQ_JOB.name()))
        .thenReturn(updateDataFromBigQueryToDB);
    submitBigQueryService.execute(STORE_ID, JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 24, 0, StringUtils.EMPTY);
    verify(queryTemplateHelper).getFullFetchQuery(JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 0, 24,
      0, StringUtils.EMPTY, 0);
    verify(gcpProperties).isDownloadFileManually();
    verify(gcpProperties).getProjectId();
    verify(updateDataFromBigQueryToDB).getResultTablePrefix();
    verify(updateDataFromBigQueryToDB, times(2)).getResultDataSet();
    verify(updateDataFromBigQueryToDB).getResultFileNamePrefix();
    verify(gcpProperties).getBulkUpdateBatchSize();
    verify(gcpProperties, times(2)).getResultGoogleCloudBucketName();
    verify(gcpProperties, times(2)).getNumberOfRecordsPerFile();
    verify(updateDataFromBigQueryToDB, times(2)).getResultFileNameLocal();
    verifyAllSubmitBigQueryProcessHelperCalls();
    verifyCacheManager(JobProcessTypes.SELLER_INFO_BQ_JOB.name());
    verify(bigQueryHelper).submitQueryToGCPAndDownloadResultsToFile(
        bigQueryRequestInfoArgumentCaptor.capture(),anyList());
    verify(fileHelper).splitFileIntoSmallFiles(
        RESULT_FILE_LOCAL, FILE_EXTENSION, NUMBER_OF_RECORDS_PER_FILE,Collections.emptyList() );
    verify(updateDataFromBigQueryToDB).writeJsonDataFromFileToDB(SPLIT_FILE_1, BULK_WRITE_BATCH_SIZE);
    verify(updateDataFromBigQueryToDB).writeJsonDataFromFileToDB(SPLIT_FILE_2, BULK_WRITE_BATCH_SIZE);
    verify(autowireCapableBeanFactory, times(8)).getBean(JobProcessTypes.SELLER_INFO_BQ_JOB.name());
    verify(fileHelper).deleteFilesFromAutoQCDirectory();
    assertEquals(RESULT_GOOGLE_CLOUD_BUCKET_NAME,
        bigQueryRequestInfoArgumentCaptor.getValue().getBucketName());
    assertEquals(RESULT_FILE_LOCAL, bigQueryRequestInfoArgumentCaptor.getValue().getLocalFileName());
    assertEquals(FILE_EXTENSION, bigQueryRequestInfoArgumentCaptor.getValue().getExtension());
    assertEquals(QUERY, bigQueryRequestInfoArgumentCaptor.getValue().getQuery());
    assertEquals(RESULT_DATA_SET, bigQueryRequestInfoArgumentCaptor.getValue().getResultDataSet());
  }

  @Test
  public void executeAndPublishEvent() throws Exception {
    when(queryTemplateHelper.getFullFetchQuery(JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 0, 24, 0,
        StringUtils.EMPTY, 0)).thenReturn(QUERY);
    when(gcpProperties.isDownloadFileManually()).thenReturn(false);
    when(autowireCapableBeanFactory.getBean(JobProcessTypes.SELLER_INFO_BQ_JOB.name()))
        .thenReturn(updateDataFromBigQueryToDB);
    when(updateDataFromBigQueryToDB.writeJsonDataFromFileToDB(anyString(), anyInt())).thenReturn(
        Collections.singletonList(new SellerFieldsChangeResponse()));
    submitBigQueryService.execute(STORE_ID, JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 24, 0,
        StringUtils.EMPTY);
    verify(queryTemplateHelper).getFullFetchQuery(JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 0, 24,
      0, StringUtils.EMPTY, 0);
    verify(gcpProperties).isDownloadFileManually();
    verify(gcpProperties).getProjectId();
    verify(updateDataFromBigQueryToDB).getResultTablePrefix();
    verify(updateDataFromBigQueryToDB, times(2)).getResultDataSet();
    verify(updateDataFromBigQueryToDB).getResultFileNamePrefix();
    verify(gcpProperties).getBulkUpdateBatchSize();
    verify(gcpProperties, times(2)).getResultGoogleCloudBucketName();
    verify(gcpProperties, times(2)).getNumberOfRecordsPerFile();
    verify(updateDataFromBigQueryToDB, times(2)).getResultFileNameLocal();
    verifyAllSubmitBigQueryProcessHelperCalls();
    verifyCacheManager(JobProcessTypes.SELLER_INFO_BQ_JOB.name());
    verify(bigQueryHelper).submitQueryToGCPAndDownloadResultsToFile(
        bigQueryRequestInfoArgumentCaptor.capture(), anyList());
    verify(fileHelper).splitFileIntoSmallFiles(
        RESULT_FILE_LOCAL, FILE_EXTENSION, NUMBER_OF_RECORDS_PER_FILE, Collections.emptyList() );
    verify(updateDataFromBigQueryToDB).writeJsonDataFromFileToDB(SPLIT_FILE_1, BULK_WRITE_BATCH_SIZE);
    verify(updateDataFromBigQueryToDB).writeJsonDataFromFileToDB(SPLIT_FILE_2, BULK_WRITE_BATCH_SIZE);
    verify(autowireCapableBeanFactory, times(8)).getBean(JobProcessTypes.SELLER_INFO_BQ_JOB.name());
    verify(fileHelper).deleteFilesFromAutoQCDirectory();
    verify(kafkaProducerService, times(2)).publishMessage(new SellerFieldsChangeResponse());
    assertEquals(RESULT_GOOGLE_CLOUD_BUCKET_NAME,
        bigQueryRequestInfoArgumentCaptor.getValue().getBucketName());
    assertEquals(RESULT_FILE_LOCAL, bigQueryRequestInfoArgumentCaptor.getValue().getLocalFileName());
    assertEquals(FILE_EXTENSION, bigQueryRequestInfoArgumentCaptor.getValue().getExtension());
    assertEquals(QUERY, bigQueryRequestInfoArgumentCaptor.getValue().getQuery());
    assertEquals(RESULT_DATA_SET, bigQueryRequestInfoArgumentCaptor.getValue().getResultDataSet());
  }

  @Test
  public void executeSellerSpecific() throws Exception {
    when(queryTemplateHelper.getFullFetchQuery(JobProcessTypes.SELLER_SPECIFIC_INFO_BQ_JOB.name(),
      0, 24, 0, StringUtils.EMPTY, 0)).thenReturn(QUERY);
    when(gcpProperties.isDownloadFileManually()).thenReturn(false);
    when(autowireCapableBeanFactory.getBean(JobProcessTypes.SELLER_SPECIFIC_INFO_BQ_JOB.name()))
        .thenReturn(dataFromBigQueryToSellerSpecificDBImpl);
    submitBigQueryService.execute(STORE_ID, JobProcessTypes.SELLER_SPECIFIC_INFO_BQ_JOB.name(), 24,
      0, StringUtils.EMPTY);
    verify(queryTemplateHelper).getFullFetchQuery(JobProcessTypes.SELLER_SPECIFIC_INFO_BQ_JOB.name(),
      0, 24, 0, StringUtils.EMPTY, 0);
    verify(gcpProperties).isDownloadFileManually();
    verify(gcpProperties).getProjectId();
    verify(dataFromBigQueryToSellerSpecificDBImpl).getResultTablePrefix();
    verify(dataFromBigQueryToSellerSpecificDBImpl, times(2)).getResultDataSet();
    verify(dataFromBigQueryToSellerSpecificDBImpl).getResultFileNamePrefix();
    verify(gcpProperties).getBulkUpdateBatchSize();
    verify(gcpProperties, times(2)).getResultGoogleCloudBucketName();
    verify(gcpProperties, times(2)).getNumberOfRecordsPerFile();
    verifyAllSubmitBigQueryProcessHelperCalls();
    verifyCacheManager(JobProcessTypes.SELLER_SPECIFIC_INFO_BQ_JOB.name());
    verify(bigQueryHelper).submitQueryToGCPAndDownloadResultsToFile(
        bigQueryRequestInfoArgumentCaptor.capture(), anyList());
    verify(fileHelper).splitFileIntoSmallFiles(SELLER_SPECIFIC_PREFIX + RESULT_FILE_LOCAL, FILE_EXTENSION,
        NUMBER_OF_RECORDS_PER_FILE, Collections.emptyList());
    verify(dataFromBigQueryToSellerSpecificDBImpl).writeJsonDataFromFileToDB(SPLIT_FILE_1, BULK_WRITE_BATCH_SIZE);
    verify(dataFromBigQueryToSellerSpecificDBImpl).writeJsonDataFromFileToDB(SPLIT_FILE_2, BULK_WRITE_BATCH_SIZE);
    verify(autowireCapableBeanFactory, times(8)).getBean(JobProcessTypes.SELLER_SPECIFIC_INFO_BQ_JOB.name());
    verify(fileHelper).deleteFilesFromAutoQCDirectory();
    assertEquals(RESULT_GOOGLE_CLOUD_BUCKET_NAME,
        bigQueryRequestInfoArgumentCaptor.getValue().getBucketName());
    assertEquals(SELLER_SPECIFIC_PREFIX + RESULT_FILE_LOCAL,
        bigQueryRequestInfoArgumentCaptor.getValue().getLocalFileName());
    assertEquals(FILE_EXTENSION, bigQueryRequestInfoArgumentCaptor.getValue().getExtension());
    assertEquals(QUERY, bigQueryRequestInfoArgumentCaptor.getValue().getQuery());
    assertEquals(SELLER_SPECIFIC_PREFIX + RESULT_DATA_SET,
        bigQueryRequestInfoArgumentCaptor.getValue().getResultDataSet());
  }

  @Test
  public void executeDuplicateProcess() {
    when(submitBigQueryProcessHelper.acquireLock(any(), anyString())).thenReturn(false);
    try {
      submitBigQueryService.execute(STORE_ID, JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 24, 0,
          StringUtils.EMPTY);
    } catch (ApplicationRuntimeException e) {
    } finally {
      verify(submitBigQueryProcessHelper).createProcess();
      verify(submitBigQueryProcessHelper).acquireLock(any(), anyString());
    }
  }

  @Test
  public void testExecuteForFullFetchQuery_ManualFileDownload() throws IOException {
    when(gcpProperties.isDownloadFileManually()).thenReturn(true);
    when(autowireCapableBeanFactory.getBean(JobProcessTypes.SELLER_INFO_BQ_JOB.name()))
        .thenReturn(updateDataFromBigQueryToDB);
    when(autowireCapableBeanFactory.getBean(JobProcessTypes.SELLER_INFO_BQ_JOB.name()))
        .thenReturn(updateDataFromBigQueryToDB);
    submitBigQueryService.execute(STORE_ID, JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 24, 0,
        StringUtils.EMPTY);
    verify(gcpProperties).isDownloadFileManually();
    verify(gcpProperties, times(2)).getNumberOfRecordsPerFile();
    verify(gcpProperties).getBulkUpdateBatchSize();
    verify(updateDataFromBigQueryToDB).getResultFileNameLocal();
    verifySubmitBigQueryProcessHelperCallsManualDownloadFile();
    verify(fileHelper).deleteFilesFromAutoQCDirectory();
    verifyCacheManager(JobProcessTypes.SELLER_INFO_BQ_JOB.name());
    verify(fileHelper).splitFileIntoSmallFiles(
        RESULT_FILE_LOCAL, FILE_EXTENSION, NUMBER_OF_RECORDS_PER_FILE, Collections.emptyList());
    verify(updateDataFromBigQueryToDB).writeJsonDataFromFileToDB(SPLIT_FILE_1, BULK_WRITE_BATCH_SIZE);
    verify(updateDataFromBigQueryToDB).writeJsonDataFromFileToDB(SPLIT_FILE_2, BULK_WRITE_BATCH_SIZE);
    verify(autowireCapableBeanFactory, times(3)).getBean(JobProcessTypes.SELLER_INFO_BQ_JOB.name());
  }

  @Test
  public void testExecute_Failed() throws Exception{
    when(queryTemplateHelper.getFullFetchQuery(JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 0, 24, 0,
        StringUtils.EMPTY, 0)).thenReturn(QUERY);
    when(gcpProperties.isDownloadFileManually()).thenReturn(false);
    doThrow(ClientException.class).when(bigQueryHelper)
        .submitQueryToGCPAndDownloadResultsToFile(any(BigQueryRequestInfo.class), anyList());
    when(autowireCapableBeanFactory.getBean(JobProcessTypes.SELLER_INFO_BQ_JOB.name()))
        .thenReturn(updateDataFromBigQueryToDB);
    submitBigQueryService.execute(STORE_ID, JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 24, 0,
        StringUtils.EMPTY);
    verify(gcpProperties).isDownloadFileManually();
    verify(updateDataFromBigQueryToDB).getResultTablePrefix();
    verify(gcpProperties).getProjectId();
    verify(updateDataFromBigQueryToDB, times(2)).getResultDataSet();
    verify(updateDataFromBigQueryToDB).getResultFileNamePrefix();
    verify(gcpProperties, times(2)).getResultGoogleCloudBucketName();
    verify(updateDataFromBigQueryToDB).getResultFileNameLocal();
    verify(queryTemplateHelper).getFullFetchQuery(JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 0, 24,
      0, StringUtils.EMPTY, 0);
    verify(bigQueryHelper).submitQueryToGCPAndDownloadResultsToFile(
        bigQueryRequestInfoArgumentCaptor.capture(), anyList());
    verify(submitBigQueryProcessHelper).createProcess();
    verify(submitBigQueryProcessHelper).acquireLock(productAnalyticsProcessArgumentCaptor.capture(), anyString());
    verify(submitBigQueryProcessHelper).updateCurrentState(productAnalyticsProcessArgumentCaptor.capture(),
        eq(SubmitBigQueryProcessSteps.DOWNLOAD_FROM_GCS));
    verify(submitBigQueryProcessHelper).updateProcessStatus(productAnalyticsProcessArgumentCaptor.capture(),
        eq(ProductAnalyticsProcessStatus.IN_PROGRESS.getStatus()));
    verify(submitBigQueryProcessHelper).updateProcessStatus(productAnalyticsProcessArgumentCaptor.capture(),
        eq(ProductAnalyticsProcessStatus.FAILED.getStatus()));
    verify(submitBigQueryProcessHelper).releaseLock(productAnalyticsProcessArgumentCaptor.capture(), anyString());
    assertEquals(ProductAnalyticsProcessStatus.FAILED.getStatus(),
        productAnalyticsProcessArgumentCaptor.getAllValues().get(4).getStatus());
    verify(autowireCapableBeanFactory, times(5)).getBean(JobProcessTypes.SELLER_INFO_BQ_JOB.name());
  }

  @Test
  public void testExecute_WriteFailed() throws Exception{
    when(queryTemplateHelper.getFullFetchQuery(JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 0, 24, 0,
        StringUtils.EMPTY, 0)).thenReturn(QUERY);
    when(gcpProperties.isDownloadFileManually()).thenReturn(false);
    when(autowireCapableBeanFactory.getBean(JobProcessTypes.SELLER_INFO_BQ_JOB.name()))
        .thenReturn(updateDataFromBigQueryToDB);
    doThrow(IOException.class).when(updateDataFromBigQueryToDB)
        .writeJsonDataFromFileToDB(anyString(), anyInt());
    submitBigQueryService.execute(STORE_ID, JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 24, 0,
        StringUtils.EMPTY);
    verify(gcpProperties).isDownloadFileManually();
    verify(gcpProperties).getProjectId();
    verify(updateDataFromBigQueryToDB).getResultTablePrefix();
    verify(updateDataFromBigQueryToDB, times(2)).getResultDataSet();
    verify(updateDataFromBigQueryToDB).getResultFileNamePrefix();
    verify(gcpProperties).getBulkUpdateBatchSize();
    verify(gcpProperties, times(2)).getResultGoogleCloudBucketName();
    verify(gcpProperties, times(2)).getNumberOfRecordsPerFile();
    verify(updateDataFromBigQueryToDB, times(2)).getResultFileNameLocal();
    verify(queryTemplateHelper).getFullFetchQuery(JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 0, 24,
      0, StringUtils.EMPTY, 0);
    verify(bigQueryHelper).submitQueryToGCPAndDownloadResultsToFile(
        bigQueryRequestInfoArgumentCaptor.capture(), anyList());
    verify(submitBigQueryProcessHelper).createProcess();
    verify(submitBigQueryProcessHelper).acquireLock(productAnalyticsProcessArgumentCaptor.capture(), anyString());
    verify(submitBigQueryProcessHelper).updateCurrentState(productAnalyticsProcessArgumentCaptor.capture(),
        eq(SubmitBigQueryProcessSteps.DOWNLOAD_FROM_GCS));
    verify(submitBigQueryProcessHelper).updateProcessStatus(productAnalyticsProcessArgumentCaptor.capture(),
        eq(ProductAnalyticsProcessStatus.IN_PROGRESS.getStatus()));
    verify(submitBigQueryProcessHelper).updateProcessStatus(productAnalyticsProcessArgumentCaptor.capture(),
        eq(ProductAnalyticsProcessStatus.FAILED.getStatus()));
    verify(submitBigQueryProcessHelper).updateCurrentState(productAnalyticsProcessArgumentCaptor.capture(),
        eq(SubmitBigQueryProcessSteps.SPLITTING_FILE));
    verify(submitBigQueryProcessHelper).updateCurrentState(productAnalyticsProcessArgumentCaptor.capture(),
        eq(SubmitBigQueryProcessSteps.WRITE_RECORDS_FROM_FILE));
    verify(submitBigQueryProcessHelper)
        .releaseLock(productAnalyticsProcessArgumentCaptor.capture(), anyString());
    verify(autowireCapableBeanFactory, times(7)).getBean(JobProcessTypes.SELLER_INFO_BQ_JOB.name());
    assertEquals(ProductAnalyticsProcessStatus.FAILED.getStatus(),
        productAnalyticsProcessArgumentCaptor.getValue().getStatus());
    verify(updateDataFromBigQueryToDB).writeJsonDataFromFileToDB(anyString(), anyInt());
    verify(fileHelper).splitFileIntoSmallFiles(
        RESULT_FILE_LOCAL, FILE_EXTENSION, NUMBER_OF_RECORDS_PER_FILE, Collections.emptyList());
  }

  @Test
  public void executeAutoApprovedProductTest() throws Exception {
    ReflectionTestUtils.setField(submitBigQueryService, "recordFetchSize", 10);
    when(autoApprovedRepository.countNumberOfRecords()).thenReturn(5L);
    when(queryTemplateHelper.getFullFetchQuery(JobProcessTypes.AUTO_APPROVED_PRODUCTS_JOB.name(),
      5, 24, 0, StringUtils.EMPTY, 0)).thenReturn(QUERY);
    when(gcpProperties.isDownloadFileManually()).thenReturn(false);
    when(autowireCapableBeanFactory.getBean(JobProcessTypes.AUTO_APPROVED_PRODUCTS_JOB.name()))
      .thenReturn(updateDataFromBigQueryToDB);
    submitBigQueryService.execute(STORE_ID, JobProcessTypes.AUTO_APPROVED_PRODUCTS_JOB.name(), 24,
      0, StringUtils.EMPTY);
    verify(queryTemplateHelper).getFullFetchQuery(JobProcessTypes.AUTO_APPROVED_PRODUCTS_JOB.name(),
      5, 24, 0, StringUtils.EMPTY, 0);
    verify(gcpProperties).isDownloadFileManually();
    verify(gcpProperties).getProjectId();
    verify(updateDataFromBigQueryToDB).getResultTablePrefix();
    verify(updateDataFromBigQueryToDB, times(2)).getResultDataSet();
    verify(updateDataFromBigQueryToDB).getResultFileNamePrefix();
    verify(gcpProperties).getBulkUpdateBatchSize();
    verify(gcpProperties, times(2)).getResultGoogleCloudBucketName();
    verify(gcpProperties, times(2)).getNumberOfRecordsPerFile();
    verify(updateDataFromBigQueryToDB, times(2)).getResultFileNameLocal();
    verifyAllSubmitBigQueryProcessHelperCalls();
    verify(bigQueryHelper).submitQueryToGCPAndDownloadResultsToFile(
      bigQueryRequestInfoArgumentCaptor.capture(),anyList());
    verify(fileHelper).splitFileIntoSmallFiles(
      RESULT_FILE_LOCAL, FILE_EXTENSION, NUMBER_OF_RECORDS_PER_FILE,Collections.emptyList() );
    verify(updateDataFromBigQueryToDB).writeJsonDataFromFileToDB(SPLIT_FILE_1, BULK_WRITE_BATCH_SIZE);
    verify(updateDataFromBigQueryToDB).writeJsonDataFromFileToDB(SPLIT_FILE_2, BULK_WRITE_BATCH_SIZE);
    verify(autowireCapableBeanFactory, times(8)).getBean(JobProcessTypes.AUTO_APPROVED_PRODUCTS_JOB.name());
    verify(fileHelper).deleteFilesFromAutoQCDirectory();
    verify(autoApprovedRepository).countNumberOfRecords();
    assertEquals(RESULT_GOOGLE_CLOUD_BUCKET_NAME,
      bigQueryRequestInfoArgumentCaptor.getValue().getBucketName());
    assertEquals(RESULT_FILE_LOCAL, bigQueryRequestInfoArgumentCaptor.getValue().getLocalFileName());
    assertEquals(FILE_EXTENSION, bigQueryRequestInfoArgumentCaptor.getValue().getExtension());
    assertEquals(QUERY, bigQueryRequestInfoArgumentCaptor.getValue().getQuery());
    assertEquals(RESULT_DATA_SET, bigQueryRequestInfoArgumentCaptor.getValue().getResultDataSet());
  }

  @Test
  public void executeAutoApprovedProductCountMoreTest() throws Exception {
    ReflectionTestUtils.setField(submitBigQueryService, "recordFetchSize", 10);
    when(autoApprovedRepository.countNumberOfRecords()).thenReturn(20L);
    submitBigQueryService.execute(STORE_ID, JobProcessTypes.AUTO_APPROVED_PRODUCTS_JOB.name(), 24,
      0, StringUtils.EMPTY);
    verify(autoApprovedRepository).countNumberOfRecords();
  }

  @Test
  public void executeSellerAnalyticsJobTest() throws Exception {
    when(queryTemplateHelper.getFullFetchQuery(JobProcessTypes.SELLER_ANALYTICS_INFO_BQ_JOB.name(),
      0, 24, 18, StringUtils.EMPTY, 0)).thenReturn(QUERY);
    when(gcpProperties.isDownloadFileManually()).thenReturn(false);
    when(autowireCapableBeanFactory.getBean(
      JobProcessTypes.SELLER_ANALYTICS_INFO_BQ_JOB.name())).thenReturn(updateDataFromBigQueryToDB);
    submitBigQueryService.execute(STORE_ID, JobProcessTypes.SELLER_ANALYTICS_INFO_BQ_JOB.name(), 24,
      18, StringUtils.EMPTY);
    verify(queryTemplateHelper).getFullFetchQuery(
        JobProcessTypes.SELLER_ANALYTICS_INFO_BQ_JOB.name(), 0, 24, 18, StringUtils.EMPTY, 0);
    verify(gcpProperties).isDownloadFileManually();
    verify(gcpProperties).getProjectId();
    verify(updateDataFromBigQueryToDB).getResultTablePrefix();
    verify(updateDataFromBigQueryToDB, times(2)).getResultDataSet();
    verify(updateDataFromBigQueryToDB).getResultFileNamePrefix();
    verify(gcpProperties).getBulkUpdateBatchSize();
    verify(gcpProperties, times(2)).getResultGoogleCloudBucketName();
    verify(gcpProperties, times(2)).getNumberOfRecordsPerFile();
    verify(updateDataFromBigQueryToDB, times(2)).getResultFileNameLocal();
    verifyAllSubmitBigQueryProcessHelperCalls();
    verify(bigQueryHelper).submitQueryToGCPAndDownloadResultsToFile(
      bigQueryRequestInfoArgumentCaptor.capture(), anyList());
    verify(fileHelper).splitFileIntoSmallFiles(RESULT_FILE_LOCAL, FILE_EXTENSION,
      NUMBER_OF_RECORDS_PER_FILE, Collections.emptyList());
    verify(updateDataFromBigQueryToDB).writeJsonDataFromFileToDB(SPLIT_FILE_1,
      BULK_WRITE_BATCH_SIZE);
    verify(updateDataFromBigQueryToDB).writeJsonDataFromFileToDB(SPLIT_FILE_2,
      BULK_WRITE_BATCH_SIZE);
    verify(autowireCapableBeanFactory, times(8)).getBean(
      JobProcessTypes.SELLER_ANALYTICS_INFO_BQ_JOB.name());
    verify(fileHelper).deleteFilesFromAutoQCDirectory();
    assertEquals(RESULT_GOOGLE_CLOUD_BUCKET_NAME,
      bigQueryRequestInfoArgumentCaptor.getValue().getBucketName());
    assertEquals(RESULT_FILE_LOCAL,
      bigQueryRequestInfoArgumentCaptor.getValue().getLocalFileName());
    assertEquals(FILE_EXTENSION, bigQueryRequestInfoArgumentCaptor.getValue().getExtension());
    assertEquals(QUERY, bigQueryRequestInfoArgumentCaptor.getValue().getQuery());
    assertEquals(RESULT_DATA_SET, bigQueryRequestInfoArgumentCaptor.getValue().getResultDataSet());
  }

  private void mockGCPProperties() {
    when(gcpProperties.getResultTablePrefix()).thenReturn(RESULT_TABLE_PREFIX);
    when(gcpProperties.getResultDataSet()).thenReturn(RESULT_DATA_SET);
    when(updateDataFromBigQueryToDB.getResultDataSet()).thenReturn(RESULT_DATA_SET);
    when(updateDataFromBigQueryToDB.getResultFileNameLocal()).thenReturn(RESULT_FILE_LOCAL);
    when(updateDataFromBigQueryToDB.getResultFileNamePrefix()).thenReturn(RESULT_FILE_NAME_PREFIX);
    when(updateDataFromBigQueryToDB.getResultTablePrefix()).thenReturn(RESULT_TABLE_PREFIX);
    when(dataFromBigQueryToSellerSpecificDBImpl.getResultDataSet()).thenReturn(SELLER_SPECIFIC_PREFIX + RESULT_DATA_SET);
    when(dataFromBigQueryToSellerSpecificDBImpl.getResultFileNameLocal()).thenReturn(SELLER_SPECIFIC_PREFIX + RESULT_FILE_LOCAL);
    when(dataFromBigQueryToSellerSpecificDBImpl.getResultFileNamePrefix()).thenReturn(SELLER_SPECIFIC_PREFIX + RESULT_FILE_NAME_PREFIX);
    when(dataFromBigQueryToSellerSpecificDBImpl.getResultTablePrefix()).thenReturn(SELLER_SPECIFIC_PREFIX + RESULT_TABLE_PREFIX);
    when(gcpProperties.getResultGoogleCloudBucketName()).thenReturn(RESULT_GOOGLE_CLOUD_BUCKET_NAME);
    when(gcpProperties.getResultFileNamePrefix()).thenReturn(RESULT_FILE_NAME_PREFIX);
    when(gcpProperties.getBulkUpdateBatchSize()).thenReturn(BULK_WRITE_BATCH_SIZE);
    when(gcpProperties.getNumberOfRecordsPerFile()).thenReturn(NUMBER_OF_RECORDS_PER_FILE);
    when(gcpProperties.getResultFileNameLocal()).thenReturn(RESULT_FILE_LOCAL);
    when(gcpProperties.getProjectId()).thenReturn(PROJECT_ID);
    when(gcpProperties.getSellerSpecificResultTablePrefix()).thenReturn(SELLER_SPECIFIC_PREFIX + RESULT_TABLE_PREFIX);
    when(gcpProperties.getSellerSpecificResultDataSet()).thenReturn(SELLER_SPECIFIC_PREFIX + RESULT_DATA_SET);
    when(gcpProperties.getSellerSpecificResultFileNamePrefix())
        .thenReturn(SELLER_SPECIFIC_PREFIX + RESULT_FILE_NAME_PREFIX);
    when(gcpProperties.getSellerSpecificResultFileNameLocal()).thenReturn(SELLER_SPECIFIC_PREFIX + RESULT_FILE_LOCAL);
  }

  private void mockSubmitBigQueryProcessHelper(){
    productAnalyticsProcess = ProductAnalyticsProcess.builder().id(PROCESS_ID).build();
    ProductAnalyticsProcess productAnalyticsProcessCopy = getProductAnalyticsProcessCopy(productAnalyticsProcess);
    when(submitBigQueryProcessHelper.createProcess()).thenReturn(getProductAnalyticsProcessCopy(productAnalyticsProcessCopy));
    when(submitBigQueryProcessHelper.acquireLock(any(ProductAnalyticsProcess.class), anyString())).thenReturn(true);
    when(submitBigQueryProcessHelper.releaseLock(any(ProductAnalyticsProcess.class), anyString())).thenReturn(true);
    productAnalyticsProcessCopy.setStatus(ProductAnalyticsProcessStatus.IN_PROGRESS.getStatus());
    when(submitBigQueryProcessHelper.updateProcessStatus(
        any(ProductAnalyticsProcess.class), eq(ProductAnalyticsProcessStatus.IN_PROGRESS.getStatus())))
        .thenReturn(getProductAnalyticsProcessCopy(productAnalyticsProcessCopy));
    productAnalyticsProcessCopy.setCurrentState(SubmitBigQueryProcessSteps.DOWNLOAD_FROM_GCS);
    when(submitBigQueryProcessHelper.updateCurrentState(
        any(ProductAnalyticsProcess.class), eq(SubmitBigQueryProcessSteps.DOWNLOAD_FROM_GCS)))
        .thenReturn(getProductAnalyticsProcessCopy(productAnalyticsProcessCopy));
    productAnalyticsProcessCopy.setCurrentState(SubmitBigQueryProcessSteps.SPLITTING_FILE);
    when(submitBigQueryProcessHelper.updateCurrentState(
        any(ProductAnalyticsProcess.class), eq(SubmitBigQueryProcessSteps.SPLITTING_FILE)))
        .thenReturn(getProductAnalyticsProcessCopy(productAnalyticsProcessCopy));
    productAnalyticsProcessCopy.setCurrentState(SubmitBigQueryProcessSteps.WRITE_RECORDS_FROM_FILE);
    when(submitBigQueryProcessHelper.updateCurrentState(
        any(ProductAnalyticsProcess.class), eq(SubmitBigQueryProcessSteps.WRITE_RECORDS_FROM_FILE)))
        .thenReturn(getProductAnalyticsProcessCopy(productAnalyticsProcessCopy));
    productAnalyticsProcessCopy.setStatus(ProductAnalyticsProcessStatus.DONE.getStatus());
    when(submitBigQueryProcessHelper.updateProcessStatus(
        any(ProductAnalyticsProcess.class), eq(ProductAnalyticsProcessStatus.DONE.getStatus())))
        .thenReturn(getProductAnalyticsProcessCopy(productAnalyticsProcessCopy));
    productAnalyticsProcessCopy.setStatus(ProductAnalyticsProcessStatus.FAILED.getStatus());
    when(submitBigQueryProcessHelper.updateProcessStatus(
        any(ProductAnalyticsProcess.class), eq(ProductAnalyticsProcessStatus.FAILED.getStatus())))
        .thenReturn(getProductAnalyticsProcessCopy(productAnalyticsProcessCopy));
  }

  private void verifySubmitBigQueryProcessHelperCallsManualDownloadFile() {
    verify(submitBigQueryProcessHelper).createProcess();
    verify(submitBigQueryProcessHelper).acquireLock(productAnalyticsProcessArgumentCaptor.capture(), anyString());
    verify(submitBigQueryProcessHelper).updateProcessStatus(
        productAnalyticsProcessArgumentCaptor.capture(), eq(ProductAnalyticsProcessStatus.IN_PROGRESS.getStatus()));
    verify(submitBigQueryProcessHelper).updateCurrentState(productAnalyticsProcessArgumentCaptor.capture(),
        eq(SubmitBigQueryProcessSteps.SPLITTING_FILE));
    verify(submitBigQueryProcessHelper).updateCurrentState(productAnalyticsProcessArgumentCaptor.capture(),
        eq(SubmitBigQueryProcessSteps.WRITE_RECORDS_FROM_FILE));
    verify(submitBigQueryProcessHelper).updateProcessStatus(productAnalyticsProcessArgumentCaptor.capture(),
        eq(ProductAnalyticsProcessStatus.DONE.getStatus()));
    verify(submitBigQueryProcessHelper).releaseLock(productAnalyticsProcessArgumentCaptor.capture(), anyString());
    productAnalyticsProcessArgumentCaptor.getAllValues().forEach(
        productAnalyticsProcess1 -> assertTrue(PROCESS_ID.equals(productAnalyticsProcess1.getId())));
  }

  private void verifyAllSubmitBigQueryProcessHelperCalls() {
    verify(submitBigQueryProcessHelper).updateCurrentState(productAnalyticsProcessArgumentCaptor.capture(),
        eq(SubmitBigQueryProcessSteps.DOWNLOAD_FROM_GCS));
    verifySubmitBigQueryProcessHelperCallsManualDownloadFile();
  }

  private void mockBigQueryCommandHelper() throws Exception {
    doNothing().when(bigQueryHelper).submitQueryToGCPAndDownloadResultsToFile(any(BigQueryRequestInfo.class), anyList());
    when(fileHelper.splitFileIntoSmallFiles(anyString(), anyString(), anyInt(),anyList()))
        .thenReturn(Arrays.asList(SPLIT_FILE_1, SPLIT_FILE_2));
    when(updateDataFromBigQueryToDB.writeJsonDataFromFileToDB(anyString(), anyInt())).thenReturn(new ArrayList<>());
  }

  private void mockCacheManager() {
    Mockito.doNothing().when(cacheServiceHelper).flushAll(JobProcessTypes.SELLER_INFO_BQ_JOB.name());
  }

  private void verifyCacheManager(String jobProcessType) {
    Mockito.verify(cacheServiceHelper).flushAll(jobProcessType);
  }


  private ProductAnalyticsProcess getProductAnalyticsProcessCopy(ProductAnalyticsProcess productAnalyticsProcess){
    ProductAnalyticsProcess newProductAnalyticsProcess = new ProductAnalyticsProcess();
    BeanUtils.copyProperties(productAnalyticsProcess, newProductAnalyticsProcess);
    return newProductAnalyticsProcess;
  }
}
