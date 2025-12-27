package com.gdn.partners.product.analytics.service.impl.helper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collections;
import java.util.zip.GZIPOutputStream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.product.analytics.model.BigQueryRequestInfo;
import com.gdn.partners.product.analytics.model.enums.FileExtensions;
import com.gdn.partners.product.analytics.properties.FileHelperProperties;
import com.gdn.partners.product.analytics.repository.AutoQCRepository;
import com.gdn.partners.product.analytics.service.helper.FileHelper;
import com.gdn.partners.product.analytics.service.impl.exception.ClientException;
import com.gdn.partners.product.analytics.service.impl.exception.FileException;
import com.google.cloud.bigquery.BigQuery;
import com.google.cloud.bigquery.BigQueryError;
import com.google.cloud.bigquery.BigQueryException;
import com.google.cloud.bigquery.BigQueryOptions;
import com.google.cloud.bigquery.Dataset;
import com.google.cloud.bigquery.DatasetId;
import com.google.cloud.bigquery.DatasetInfo;
import com.google.cloud.bigquery.Job;
import com.google.cloud.bigquery.JobInfo;
import com.google.cloud.bigquery.JobStatus;
import com.google.cloud.bigquery.TableId;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Storage;


public class BigQueryHelperImplTest {

  @Mock
  private Storage storage;

  @Mock
  private BigQuery bigQuery;

  @Mock
  private FileHelperProperties fileHelperProperties;

  @Mock
  private AutoQCRepository autoQCRepository;

  @Mock
  private FileHelper fileHelper;

  @InjectMocks
  private BigQueryHelperImpl bigQueryHelper;

  private static final String PROJECT_ID = "projectId";
  private static final String GCS_FILE_PREFIX = "gcsFilePrefix";
  private static final String RESULT_DATA_SET = "resultDataSet";
  private static final String RESULT_TABLE = "resultTable";
  private static final String RESULT_FILE_NAME_LOCAL = "result";
  private static final String BUCKET_NAME = "bucketName";
  private static final String BUCKET_URI = "bucketUri";
  private static final String REASON = "REASON";
  private static final String FILE_EXTENSION = FileExtensions.NDJSON.getExtension();
  private static final String QUERY = "SELECT * FROM somewhere";
  private static final String JSON_FILE = "jsonFile";
  private static final String SAMPLE_CONTENT = "sample content for an uncompressed file";
  private static BigQueryRequestInfo bigQueryRequestInfo;
  private static File actualUncompressedFile;
  private static File compressedFile;
  private static File jsonFile;
  private static File unSplitFile;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    bigQueryRequestInfo = BigQueryRequestInfo.builder()
        .localFileName(RESULT_FILE_NAME_LOCAL)
        .extension(FILE_EXTENSION)
        .query(QUERY)
        .resultDataSet(RESULT_DATA_SET)
        .destinationTableId(TableId.of(RESULT_DATA_SET, RESULT_TABLE))
        .gcsFilePrefix(GCS_FILE_PREFIX)
        .bucketName(BUCKET_NAME)
        .bucketUri(BUCKET_URI)
        .build();
    when(fileHelperProperties.getBufferSize()).thenReturn(4);
    when(fileHelperProperties.getDirectory()).thenReturn("/target/");
    compressedFile = new File(RESULT_FILE_NAME_LOCAL + FileExtensions.NDJSON.getExtension() +
        FileExtensions.GZIP.getExtension());
    actualUncompressedFile = new File(RESULT_FILE_NAME_LOCAL + FileExtensions.NDJSON.getExtension());
    jsonFile = new File(JSON_FILE);
    unSplitFile = new File(RESULT_FILE_NAME_LOCAL + FileExtensions.NDJSON.getExtension());

    lenient().when(bigQuery.create(any(JobInfo.class))).thenReturn(mock(Job.class));
    lenient().when(bigQuery.create(any(DatasetInfo.class))).thenReturn(mock(Dataset.class));
    lenient().when(bigQuery.getOptions()).thenReturn(mock(BigQueryOptions.class));

  }

  @AfterEach
  public void tearDown() throws Exception {
    compressedFile.delete();
    actualUncompressedFile.delete();
    jsonFile.delete();
    unSplitFile.delete();
    Mockito.verifyNoMoreInteractions(bigQuery);
    Mockito.verifyNoMoreInteractions(storage);
    Mockito.verifyNoMoreInteractions(fileHelperProperties);
    Mockito.verifyNoMoreInteractions(autoQCRepository);
    Mockito.verifyNoMoreInteractions(fileHelper);
  }

  @Test
  public void submitQueryToGCPAndDownloadResultsToFile() throws Exception {
    mockFiles();
    mockDataSetCreationBigQuery();
    mockGoogleCloudStorage();
    mockSuccessfulBigQueryJob();
    compressExpectedUncompressedFile();
    bigQueryHelper.submitQueryToGCPAndDownloadResultsToFile(bigQueryRequestInfo, Collections.emptyList());
    verify(bigQuery, times(2)).create(any(JobInfo.class));
    verify(bigQuery).getOptions();
    verify(bigQuery).getDataset(any(DatasetId.class));
    verify(storage).list(eq(BUCKET_NAME),
        any(Storage.BlobListOption.class));
    Mockito.verify(fileHelper).unZipFile(anyString(), anyString(), Mockito.anyList());
    Mockito.verify(fileHelperProperties, Mockito.times(1)).getDirectory();
    Mockito.verify(fileHelperProperties, Mockito.times(1)).getBufferSize();
  }

  @Test
  public void submitQueryToGCPAndDownloadResultsToFileNoDataSet() throws Exception {
    mockFiles();
    BigQueryOptions bigQueryOptions = mock(BigQueryOptions.class);
    when(bigQueryOptions.getProjectId()).thenReturn(PROJECT_ID);
    when(bigQuery.getOptions()).thenReturn(bigQueryOptions);
    mockGoogleCloudStorage();
    mockSuccessfulBigQueryJob();
    compressExpectedUncompressedFile();
    bigQueryHelper.submitQueryToGCPAndDownloadResultsToFile(bigQueryRequestInfo, Collections.emptyList());
    verify(bigQuery, times(2)).create(any(JobInfo.class));
    verify(bigQuery).getOptions();
    verify(bigQuery).getDataset(any(DatasetId.class));
    verify(bigQuery).create(any(DatasetInfo.class));
    verify(storage).list(eq(BUCKET_NAME),
        any(Storage.BlobListOption.class));
    Mockito.verify(fileHelper).unZipFile(anyString(), anyString(), Mockito.anyList());
    Mockito.verify(fileHelperProperties, Mockito.times(1)).getDirectory();
    Mockito.verify(fileHelperProperties, Mockito.times(1)).getBufferSize();
  }

  @Test
  public void submitQueryToGCPAndDownloadResultsToFileNoDataSetException() throws Exception {
    mockFiles();
    BigQueryOptions bigQueryOptions = mock(BigQueryOptions.class);
    when(bigQueryOptions.getProjectId()).thenReturn(PROJECT_ID);
    when(bigQuery.getOptions()).thenReturn(bigQueryOptions);
    when(bigQuery.create(any(DatasetInfo.class)))
        .thenThrow(new BigQueryException(1, REASON, new BigQueryError(REASON, REASON, REASON)));
    mockGoogleCloudStorage();
    mockSuccessfulBigQueryJob();
    compressExpectedUncompressedFile();
    try {
      bigQueryHelper.submitQueryToGCPAndDownloadResultsToFile(bigQueryRequestInfo, Collections.emptyList());
    } catch (ClientException e) {
    } finally {
      verify(bigQuery).getOptions();
      verify(bigQuery).getDataset(any(DatasetId.class));
      verify(bigQuery).create(any(DatasetInfo.class));
      Mockito.verify(fileHelperProperties, Mockito.times(1)).getBufferSize();
    }
  }

  @Test
  public void submitQueryToGCPAndDownloadResultsToFile_FailedQuery_Test() throws Exception {
    mockDataSetCreationBigQuery();
    mockFailureBigQueryJob();
    try {
      bigQueryHelper.submitQueryToGCPAndDownloadResultsToFile(bigQueryRequestInfo, Collections.emptyList());
    }catch (Exception e){
      assertEquals(RuntimeException.class, e.getClass());
    }finally {
      verify(bigQuery).create(any(JobInfo.class));
      verify(bigQuery).getOptions();
      verify(bigQuery).getDataset(any(DatasetId.class));
    }
  }

  @Test
  public void submitQueryToGCPAndDownloadResultsToFile_FailedQuery_Test_1() throws Exception {
    mockDataSetCreationBigQuery();
    Job job = mock(Job.class);
    when(bigQuery.create(any(JobInfo.class))).thenReturn(job).thenThrow(RuntimeException.class);
    try {
      bigQueryHelper.submitQueryToGCPAndDownloadResultsToFile(bigQueryRequestInfo, Collections.emptyList());
    } catch (Exception e) {
      assertEquals(ClientException.class, e.getClass());
    } finally {
      verify(bigQuery).create(any(JobInfo.class));
      verify(bigQuery).getOptions();
      verify(bigQuery).getDataset(any(DatasetId.class));
    }
  }

  @Test
  public void submitQueryToGCPAndDownloadResultsToFile_EmptyJob_Test() throws Exception {
    mockDataSetCreationBigQuery();
    mockEmptyBigQueryJob();
    try {
      bigQueryHelper.submitQueryToGCPAndDownloadResultsToFile(bigQueryRequestInfo, Collections.emptyList());
    } catch (Exception e){
      assertEquals(ClientException.class, e.getClass());
    } finally {
      verify(bigQuery).create(any(JobInfo.class));
      verify(bigQuery).getOptions();
      verify(bigQuery).getDataset(any(DatasetId.class));
    }
  }

  @Test
  public void submitQueryToGCPAndDownloadResultsToFileExceptionCase() throws Exception {
    mockFiles();
    mockDataSetCreationBigQuery();
    mockGoogleCloudStorage();
    mockSuccessfulBigQueryJob();
    when(fileHelperProperties.getBufferSize()).thenThrow(RuntimeException.class);
    try {
      bigQueryHelper.submitQueryToGCPAndDownloadResultsToFile(bigQueryRequestInfo, Collections.emptyList());
    } catch (FileException e) {
    } finally {
      verify(bigQuery, times(2)).create(any(JobInfo.class));
      verify(bigQuery).getOptions();
      verify(bigQuery).getDataset(any(DatasetId.class));
      verify(storage).list(eq(BUCKET_NAME), any(Storage.BlobListOption.class));
      Mockito.verify(fileHelperProperties, Mockito.times(1)).getDirectory();
      Mockito.verify(fileHelper).unZipFile(anyString(), anyString(), Mockito.anyList());
    }
  }

  private void mockFiles() throws IOException {
    FileWriter fileWriter = new FileWriter(compressedFile);
    fileWriter.write(SAMPLE_CONTENT);
    fileWriter.flush();
    fileWriter.close();
    FileWriter fileWriter1 = new FileWriter(actualUncompressedFile);
    fileWriter1.write(SAMPLE_CONTENT);
    fileWriter1.flush();
    fileWriter1.close();
  }

  private void mockSuccessfulBigQueryJob() throws InterruptedException{
    Job job = mock(Job.class);
    when(bigQuery.create(any(JobInfo.class))).thenReturn(job);
    when(job.isDone()).thenReturn(Boolean.TRUE);
    JobStatus jobStatus = mock(JobStatus.class);
    when(job.getStatus()).thenReturn(jobStatus);
    when(jobStatus.getError()).thenReturn(null);
    when(job.waitFor()).thenReturn(job);
  }

  private void mockGoogleCloudStorage(){
    com.google.api.gax.paging.Page<Blob> blobPage = mock(com.google.api.gax.paging.Page.class);
    when(storage.list(anyString(), any(Storage.BlobListOption.class))).thenReturn(blobPage);
    Blob blob = mock(Blob.class);
    String csv = "hi, hello";
    when(blob.getContent()).thenReturn(csv.getBytes());
    when(blobPage.getValues()).thenReturn(Collections.singleton(blob));
  }

  private void mockDataSetCreationBigQuery() {
    BigQueryOptions bigQueryOptions = mock(BigQueryOptions.class);
    when(bigQueryOptions.getProjectId()).thenReturn(PROJECT_ID);
    when(bigQuery.getOptions()).thenReturn(bigQueryOptions);
    Dataset dataset = mock(Dataset.class);
    when(bigQuery.getDataset(any(DatasetId.class))).thenReturn(dataset);
  }

  private void compressExpectedUncompressedFile() throws IOException {
    byte[] buffer = new byte[fileHelperProperties.getBufferSize()];
    FileOutputStream fileOutputStream = new FileOutputStream(compressedFile);
    GZIPOutputStream gzipOutputStream = new GZIPOutputStream(fileOutputStream);
    FileInputStream fileInputStream = new FileInputStream(actualUncompressedFile);

    int bytes_read;
    while ((bytes_read = fileInputStream.read(buffer)) > 0) {
      gzipOutputStream.write(buffer, 0, bytes_read);
    }
    fileInputStream.close();
    gzipOutputStream.finish();
    gzipOutputStream.close();
  }

  private void mockFailureBigQueryJob() {
    doThrow(RuntimeException.class).when(bigQuery).create(any(JobInfo.class));
  }

  private void mockEmptyBigQueryJob() throws InterruptedException {
    Job job = mock(Job.class);
    when(bigQuery.create(any(JobInfo.class))).thenReturn(job);
    when(job.isDone()).thenReturn(Boolean.TRUE);
    when(job.waitFor()).thenReturn(null);
  }



}