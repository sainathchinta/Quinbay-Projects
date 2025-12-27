package com.gdn.partners.product.analytics.service.impl.helper;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import com.gdn.partners.product.analytics.model.BigQueryRequestInfo;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.enums.FileExtensions;
import com.gdn.partners.product.analytics.properties.FileHelperProperties;
import com.gdn.partners.product.analytics.service.helper.BigQueryHelper;
import com.gdn.partners.product.analytics.service.helper.FileHelper;
import com.gdn.partners.product.analytics.service.impl.exception.ClientException;
import com.google.cloud.bigquery.BigQuery;
import com.google.cloud.bigquery.BigQueryException;
import com.google.cloud.bigquery.Dataset;
import com.google.cloud.bigquery.DatasetId;
import com.google.cloud.bigquery.DatasetInfo;
import com.google.cloud.bigquery.ExtractJobConfiguration;
import com.google.cloud.bigquery.FormatOptions;
import com.google.cloud.bigquery.Job;
import com.google.cloud.bigquery.JobConfiguration;
import com.google.cloud.bigquery.JobId;
import com.google.cloud.bigquery.JobInfo;
import com.google.cloud.bigquery.QueryJobConfiguration;
import com.google.cloud.bigquery.TableId;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Storage;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BigQueryHelperImpl implements BigQueryHelper {

  private static final String RESULT_FILE_COMPRESSION = "GZIP";

  @Autowired
  private BigQuery bigQuery;

  @Autowired
  @Qualifier("googleCloudStorage")
  private Storage googleCloudStorage;

  @Autowired
  private FileHelperProperties fileHelperProperties;

  @Autowired
  private FileHelper fileHelper;

  @Override
  public void submitQueryToGCPAndDownloadResultsToFile(BigQueryRequestInfo bigQueryRequestInfo, List<String> localFilePathList)
      throws Exception {
    createResultDataSetIfNotExists(bigQueryRequestInfo);
    runQueryJob(bigQueryRequestInfo.getQuery(), bigQueryRequestInfo.getDestinationTableId());
    runExtractJob(bigQueryRequestInfo.getDestinationTableId(), bigQueryRequestInfo.getBucketUri());
    //list of compressed Files
    List<String> storageBlobToLocalByBlobNamePrefix =
      downloadGoogleCloudStorageBlobToLocalByBlobNamePrefix(bigQueryRequestInfo.getGcsFilePrefix(),
        bigQueryRequestInfo.getLocalFileName(), bigQueryRequestInfo.getBucketName());
    localFilePathList.addAll(fileHelper.unZipFile(bigQueryRequestInfo.getLocalFileName(),
      bigQueryRequestInfo.getExtension(), storageBlobToLocalByBlobNamePrefix));
  }


  /**
   * Creating result data set in bigQuery if it doesn't exist
   *
   * @param bigQueryRequestInfo
   */
  private void createResultDataSetIfNotExists(BigQueryRequestInfo bigQueryRequestInfo) throws ClientException {
    DatasetId datasetId = DatasetId.of(bigQuery.getOptions().getProjectId(), bigQueryRequestInfo.getResultDataSet());
    Dataset dataset = bigQuery.getDataset(datasetId);
    if (Objects.isNull(dataset)) {
      log.info("Result Dataset doesn't exist. Creating...");
      DatasetInfo datasetInfo = DatasetInfo.newBuilder(bigQueryRequestInfo.getResultDataSet()).build();
      try {
        bigQuery.create(datasetInfo);
      } catch (BigQueryException e) {
        throw new ClientException(e.getError().getMessage(), e);
      }
    }
  }

  /**
   * Runs BigQuery query job for given query and saves result to the table with id as given resultTableId
   *
   * @param query
   * @param resultTableId
   * @throws InterruptedException
   */
  private void runQueryJob(String query, TableId resultTableId) throws InterruptedException, ClientException {
    QueryJobConfiguration queryConfig =
        QueryJobConfiguration.newBuilder(query).setUseLegacySql(Boolean.FALSE).setDestinationTable(resultTableId)
            .build();


    log.debug("BigQuery query job started");
    runBigQueryJob(queryConfig);
  }

  /**
   * Run BigQueryJob with given query configuration
   *
   * @param queryConfig - configuration of the job
   * @throws InterruptedException
   */
  private void runBigQueryJob(JobConfiguration queryConfig) throws InterruptedException, ClientException {
    JobId jobId = JobId.of(UUID.randomUUID().toString());
    Job queryJob = bigQuery.create(JobInfo.newBuilder(queryConfig).setJobId(jobId).build());
    Job completedQueryJob = queryJob.waitFor();

    Optional.ofNullable(completedQueryJob).orElseThrow(() -> new ClientException(Constants.JOB_NOT_EXISTS));
    Optional.ofNullable(completedQueryJob.getStatus())
        .orElseThrow(() -> new ClientException(Constants.STATUS_NOT_EXISTS));
    if (Objects.nonNull(completedQueryJob.getStatus().getError())) {
      throw new ClientException(completedQueryJob.getStatus().getError().toString());
    }
  }

  /**
   * Runs BigQuery extract job to export the record of table with id given resultTableId
   * to given bucketUri of Google Cloud Storage
   *
   * @param resultTableId
   * @param bucketUri
   * @throws InterruptedException
   */
  private void runExtractJob(TableId resultTableId, String bucketUri) throws InterruptedException, ClientException {
    ExtractJobConfiguration extractJobConfiguration =
        ExtractJobConfiguration.newBuilder(resultTableId, bucketUri).setCompression(RESULT_FILE_COMPRESSION)
            .setFormat(FormatOptions.json().getType()).build();
    log.debug("BigQuery extract job started");
    runBigQueryJob(extractJobConfiguration);
  }

  /**
   * Download Google Cloud Storage blobs to local by prefix
   *
   * @param blobNamePrefix
   * @param localFileName
   * @param bucketName
   */
  private List<String> downloadGoogleCloudStorageBlobToLocalByBlobNamePrefix(String blobNamePrefix, String localFileName, String bucketName) {
    List<String> compressedFileNameList = new ArrayList<>();
    try {
      com.google.api.gax.paging.Page<Blob> blobList =
        googleCloudStorage.list(bucketName, Storage.BlobListOption.prefix(blobNamePrefix));
      int sequence = 0;
      for (Blob blob : blobList.getValues()) {
        String localFilePath = getGZipFilePath(localFileName, FileExtensions.NDJSON.getExtension(), sequence);
        log.info("Downloading results from {} to file {}", blobNamePrefix, localFilePath);
        File file = new File(localFilePath);
        file.getParentFile().mkdirs();
        Path localFileNew = Paths.get(localFilePath);
        log.debug("blob {} to localFile {} ", blob, localFileNew);
        blob.downloadTo(localFileNew);
        sequence++;
        compressedFileNameList.add(localFilePath);
      }
    } catch (Exception e) {
      log.error("Error occurred while downloading from Google Cloud Storage: ", e);
    }
    return compressedFileNameList;
  }

  private String getGZipFilePath(String fileName, String extension, int sequence) {
    return new StringBuilder(fileHelperProperties.getDirectory()).append(fileName).append(sequence).append(extension)
        .append(FileExtensions.GZIP.getExtension()).toString();
  }
}
