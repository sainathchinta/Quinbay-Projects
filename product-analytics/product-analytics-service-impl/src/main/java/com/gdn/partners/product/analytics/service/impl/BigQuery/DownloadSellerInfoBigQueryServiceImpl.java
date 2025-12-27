package com.gdn.partners.product.analytics.service.impl.BigQuery;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.gdn.partners.product.analytics.model.enums.JobProcessTypes;
import com.gdn.partners.product.analytics.repository.AutoApprovedRepository;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcess;
import com.gdn.partners.product.analytics.model.BigQueryRequestInfo;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.SubmitBigQueryProcessSteps;
import com.gdn.partners.product.analytics.model.enums.FileExtensions;
import com.gdn.partners.product.analytics.model.enums.ProductAnalyticsProcessStatus;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.service.KafkaProducerService;
import com.gdn.partners.product.analytics.service.UpdateDataFromBigQueryToDB;
import com.gdn.partners.product.analytics.service.bigQuery.DownloadSellerInfoBigQueryService;
import com.gdn.partners.product.analytics.service.bigQuery.ProductAnalyticsProcessService;
import com.gdn.partners.product.analytics.service.helper.BigQueryHelper;
import com.gdn.partners.product.analytics.service.helper.CacheServiceHelper;
import com.gdn.partners.product.analytics.service.helper.FileHelper;
import com.gdn.partners.product.analytics.service.impl.exception.ClientException;
import com.gdn.partners.product.analytics.service.impl.exception.FileException;
import com.gdn.partners.product.analytics.service.impl.helper.QueryTemplateHelper;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.google.cloud.bigquery.TableId;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class DownloadSellerInfoBigQueryServiceImpl implements DownloadSellerInfoBigQueryService {

  @Autowired
  private GCPProperties gcpProperties;

  @Autowired
  private QueryTemplateHelper queryTemplateHelper;

  @Autowired
  private BigQueryHelper bigQueryHelper;

  @Autowired
  private FileHelper fileHelper;

  @Autowired
  private ProductAnalyticsProcessService productAnalyticsProcessService;

  @Autowired
  private CacheServiceHelper cacheServiceHelper;

  @Autowired
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Autowired
  private KafkaProducerService kafkaProducerService;

  @Autowired
  private AutoApprovedRepository autoApprovedRepository;

  @Value("${record.fetch.size}")
  private int recordFetchSize;

  @Value("${auto.approved.fetch.hour}")
  private int autoApprovalFetchHour;

  @Override
  @Async
  public void execute(String storeId, String jobProcessType, int fetchHourThreshold,
    int suggestedDateFetch, String attributeName) {
    int numberOfRecordsInDb = 0;
    if (JobProcessTypes.AUTO_APPROVED_PRODUCTS_JOB.name().equals(jobProcessType)) {
      numberOfRecordsInDb = (int) autoApprovedRepository.countNumberOfRecords();
      if (recordFetchSize - numberOfRecordsInDb < 0) {
        log.warn("Record size more than maximum allowed records. Exiting the job.");
        return;
      }
    }
    log.info("Starting process for jobType : {} ", jobProcessType);
    //Create new process
    ProductAnalyticsProcess process = productAnalyticsProcessService.createProcess();
    List<String> localFilePathList = new ArrayList<>();
    try {
      // Acquire lock and throw exception if some other process is already running
      if (!productAnalyticsProcessService.acquireLock(process, jobProcessType)) {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, Constants.PROCESS_LOCKED);
      }
      try {
        // Update process status to in progress
        process = productAnalyticsProcessService
            .updateProcessStatus(process, ProductAnalyticsProcessStatus.IN_PROGRESS.getStatus());

        log.info("Process to download file from gcp for job : {} ", jobProcessType);

        // Start download process
        process = downloadFileFromGCP(process, jobProcessType, localFilePathList,
            recordFetchSize - numberOfRecordsInDb, fetchHourThreshold, suggestedDateFetch,
            attributeName);

        log.info("Process to download file from gcp for job : {} done!", jobProcessType);

        // Start writing to file
        Pair<ProductAnalyticsProcess, List<SellerFieldsChangeResponse>> responseAfterSavingInDb =
            writeRecordsFromResultFileToDB(process, jobProcessType, localFilePathList);

        log.info("Process to write data to db for job : {} done!", jobProcessType);

        process = responseAfterSavingInDb.getLeft();

        // Clear cache
        if (!(JobProcessTypes.AUTO_APPROVED_PRODUCTS_JOB.name().equals(jobProcessType)
          || JobProcessTypes.SELLER_ANALYTICS_INFO_BQ_JOB.name().equals(jobProcessType))) {
          cacheServiceHelper.flushAll(jobProcessType);
        }

        log.info("Process to flush cache for job : {} done!", jobProcessType);

        if (CollectionUtils.isNotEmpty(responseAfterSavingInDb.getRight())) {
          responseAfterSavingInDb.getRight()
              .forEach(sellerFieldsChangeResponse -> kafkaProducerService.publishMessage(sellerFieldsChangeResponse));
        }

        log.info("Process to publish messages for job : {} done!", jobProcessType);

        // Update bigquery process as done
        process =
            productAnalyticsProcessService.updateProcessStatus(process, ProductAnalyticsProcessStatus.DONE.getStatus());

        fileHelper.deleteFilesFromAutoQCDirectory();

        // Release lock
        productAnalyticsProcessService.releaseLock(process, jobProcessType);
        log.info("Process completed for jobType : {} ", jobProcessType);
      } catch (Exception e) {
        log.error("Exception on bigQuery job execution : {} ", process, e);
        ProductAnalyticsProcess productAnalyticsProcess = productAnalyticsProcessService
            .updateProcessStatus(process, ProductAnalyticsProcessStatus.FAILED.getStatus());
        productAnalyticsProcessService.releaseLock(productAnalyticsProcess, jobProcessType);
      }
    } catch (ApplicationRuntimeException e) {
      log.error("Acquire lock failed for the process : {} processType : {}", process, jobProcessType, e);
    }

  }

  private UpdateDataFromBigQueryToDB getUpdateToDBImplByJobProcessType(String jobProcessType) {
    return (UpdateDataFromBigQueryToDB) this.autowireCapableBeanFactory.getBean(jobProcessType);
  }

  private Pair<ProductAnalyticsProcess, List<SellerFieldsChangeResponse>> writeRecordsFromResultFileToDB(
      ProductAnalyticsProcess productAnalyticsProcess, String jobProcessType, List<String> localFilePathList) {
    productAnalyticsProcess = productAnalyticsProcessService
        .updateCurrentState(productAnalyticsProcess, SubmitBigQueryProcessSteps.SPLITTING_FILE);
    log.info("Splitting file into smaller files");

    List<String> splitFiles = fileHelper.splitFileIntoSmallFiles(
        getUpdateToDBImplByJobProcessType(jobProcessType).getResultFileNameLocal(), FileExtensions.NDJSON.getExtension(),
        gcpProperties.getNumberOfRecordsPerFile(), localFilePathList);
    log.info("No. of split files: {}, records per file: {}", splitFiles.size(),
        gcpProperties.getNumberOfRecordsPerFile());

    productAnalyticsProcess = productAnalyticsProcessService
        .updateCurrentState(productAnalyticsProcess, SubmitBigQueryProcessSteps.WRITE_RECORDS_FROM_FILE);

    log.info("Writing the records to database started");
    int batchSize = gcpProperties.getBulkUpdateBatchSize();
    List<SellerFieldsChangeResponse> sellerFieldsChangeResponses = new ArrayList<>();
    splitFiles.forEach(file -> {
      try {
        sellerFieldsChangeResponses.addAll(
            getUpdateToDBImplByJobProcessType(jobProcessType).writeJsonDataFromFileToDB(file, batchSize));
      } catch (IOException e) {
        throw new FileException(e.getMessage(), e);
      }
    });
    return Pair.of(productAnalyticsProcess, sellerFieldsChangeResponses);
  }

  private ProductAnalyticsProcess downloadFileFromGCP(
    ProductAnalyticsProcess productAnalyticsProcess, String jobProcessType,
    List<String> localFilePathList, int deltaRecordSize, int fetchHourThreshold,
    int suggestedDateFetch, String attributeName) {
    if (!gcpProperties.isDownloadFileManually()) {
      productAnalyticsProcess  = productAnalyticsProcessService
          .updateCurrentState(productAnalyticsProcess, SubmitBigQueryProcessSteps.DOWNLOAD_FROM_GCS);
      String query =
        getQuery(jobProcessType, deltaRecordSize, fetchHourThreshold, suggestedDateFetch,
          attributeName, autoApprovalFetchHour);
      log.debug("Query: {}", query);
      Long timeStampNow = DateTime.now().getMillis();
      String resultTableNameWithTimeStamp =
          getResultTableNameWithTimeStamp(timeStampNow, jobProcessType);
      TableId resultTableId = TableId
          .of(gcpProperties.getProjectId(), getUpdateToDBImplByJobProcessType(jobProcessType).getResultDataSet(),
              resultTableNameWithTimeStamp);

      String resultFilePrefixWithTimeStamp =
          getResultFilePrefixWithTimeStamp(timeStampNow, jobProcessType);
      String gcsFileName = getResultFileNameWithWildCard(resultFilePrefixWithTimeStamp);
      String bucketUri = getResultFileGoogleCloudStorageBucketUri(gcsFileName);

      BigQueryRequestInfo bigQueryRequestInfo =
          getBigQueryRequestInfo(query, resultTableId, resultFilePrefixWithTimeStamp, bucketUri, jobProcessType);
      try {
        bigQueryHelper.submitQueryToGCPAndDownloadResultsToFile(bigQueryRequestInfo, localFilePathList);
      } catch (Exception e) {
        throw new ClientException(e.getMessage(), e);
      }
    }
    return productAnalyticsProcess;
  }

  private BigQueryRequestInfo getBigQueryRequestInfo(String query, TableId resultTableId,
      String resultFilePrefixWithTimeStamp, String bucketUri, String jobProcessType) {
    return BigQueryRequestInfo.builder()
        .localFileName(getUpdateToDBImplByJobProcessType(jobProcessType).getResultFileNameLocal())
        .extension(FileExtensions.NDJSON.getExtension()).query(query)
        .resultDataSet(getUpdateToDBImplByJobProcessType(jobProcessType).getResultDataSet())
        .destinationTableId(resultTableId).gcsFilePrefix(resultFilePrefixWithTimeStamp)
        .bucketName(gcpProperties.getResultGoogleCloudBucketName()).bucketUri(bucketUri).build();
  }

  private String getQuery(String jobProcessType, int recordFetchSize, int fetchHourThreshold,
    int suggestedDateFetch, String attributeName, int autoApprovalFetchHour) {
    return queryTemplateHelper.getFullFetchQuery(jobProcessType, recordFetchSize, fetchHourThreshold,
      suggestedDateFetch, attributeName, autoApprovalFetchHour);
  }

  /**
   * Gives the tableName to which the query results are saved in bigQuery
   * @param timeStampNow - Appends timeStamp to a defined prefix
   * @param jobProcessType
   * @return String
   */
  private String getResultTableNameWithTimeStamp(Long timeStampNow, String jobProcessType) {
    return new StringBuilder(getUpdateToDBImplByJobProcessType(jobProcessType).getResultTablePrefix())
        .append(Constants.UNDERSCORE_SEPARATOR).append(timeStampNow).toString();
  }

  /**
   * Gives timeStamp appended blob prefix
   * @param timeStampNow
   * @param jobProcessType
   * @return String
   */
  private String getResultFilePrefixWithTimeStamp(Long timeStampNow, String jobProcessType) {
    return new StringBuilder(getUpdateToDBImplByJobProcessType(jobProcessType).getResultFileNamePrefix())
        .append(Constants.UNDERSCORE_SEPARATOR).append(timeStampNow).toString();
  }

  /**
   * Gives wildcard appended path of the blob
   * @param resultFilePrefixWithTimeStamp - timestamp appended blob prefix
   * @return String
   */
  private String getResultFileNameWithWildCard(String resultFilePrefixWithTimeStamp) {
    return new StringBuilder(resultFilePrefixWithTimeStamp)
        .append(Constants.UNDERSCORE_SEPARATOR)
        .append(Constants.WILDCARD)
        .append(FileExtensions.NDJSON.getExtension())
        .append(FileExtensions.GZIP.getExtension())
        .toString();
  }

  /**
   * Gives the full Google Cloud BucketUri to which the results table is extracted to
   * @param fileName - WildCard path of the blob
   * @return String
   */
  private String getResultFileGoogleCloudStorageBucketUri(String fileName) {
    return new StringBuilder(Constants.GOOGLE_CLOUD_STORAGE_URI_PREFIX)
        .append(gcpProperties.getResultGoogleCloudBucketName())
        .append(Constants.SLASH_SEPARATOR)
        .append(fileName)
        .toString();
  }
}
