package com.gdn.mta.bulk.service;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.gdn.partners.bulk.util.Constant;
import com.google.cloud.ReadChannel;
import com.google.cloud.storage.BlobId;
import com.google.cloud.storage.CopyWriter;
import org.apache.commons.codec.binary.Base64;
import org.apache.poi.util.IOUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;

public class GCSServiceBeanTest {

  @InjectMocks
  private GCSServiceBean gcsServiceBean;

  @Mock
  private Storage googleCloudStorage;

  @Mock
  private Bucket bucket;

  @Mock
  private ReadChannel readChannel;

  @Mock
  private Blob blob;

  private ProfileResponse profileResponse;
  private static String DEFAULT_BUSINESS_PARTNER_CODE = "BP-10001";
  private static String gcsUnifiedTemplateDirectory = "/temp";
  private static String BUCKET_NAME = "BUCKET-NAME";
  private static String SOURCE_PATH = "SOURCE";
  private static String DESTINATION_PATH = "DESTINATION";
  private static final String FILE_PATH = "path/to/file.xlsx";

  private Map<String, String> getFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(
      Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "Blibli-mass-base-template.xlsm"))),
      "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
  }

  @Test
  public void uploadCreatedFileTest() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(googleCloudStorage.get(Mockito.anyString(), Mockito.any())).thenReturn(bucket);
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(excelFile)));
    Mockito.when(bucket.create("/temp", excelFile, contentType)).thenReturn(blob);
    Blob blob = gcsServiceBean.uploadCreatedFile(bucket, gcsUnifiedTemplateDirectory, excelFile);
    Assertions.assertNotNull(blob);
  }

  @Test
  public void uploadCreatedFileFailedTest() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(googleCloudStorage.get(Mockito.anyString(), Mockito.any())).thenReturn(bucket);
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(excelFile)));
    Mockito.when(bucket.create("/temp/BP-10001/general_template.xlsm", excelFile, contentType))
      .thenReturn(null);
    Blob blob = gcsServiceBean.uploadCreatedFile(bucket, gcsUnifiedTemplateDirectory, excelFile);
  }

  @Test
  public void downloadSourceFileTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, gcsUnifiedTemplateDirectory)).thenReturn(blob);
    gcsServiceBean.downloadFile(BUCKET_NAME, gcsUnifiedTemplateDirectory);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, gcsUnifiedTemplateDirectory);
  }

  @Test
  public void openFileStreamTest() {
    // Mock behavior
    when(googleCloudStorage.get(BUCKET_NAME, FILE_PATH)).thenReturn(blob);
    when(blob.reader()).thenReturn(readChannel);

    // Invoke method
    InputStream inputStream = gcsServiceBean.openFileStream(BUCKET_NAME, FILE_PATH);

    // Assert
    Assertions.assertNotNull(inputStream);
    verify(googleCloudStorage).get(BUCKET_NAME, FILE_PATH);
    verify(blob).reader();
  }

  @Test
  public void openFileStream_NullBlob_ThrowsException() {
    when(googleCloudStorage.get(BUCKET_NAME, FILE_PATH)).thenReturn(null);

    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> gcsServiceBean.openFileStream(BUCKET_NAME, FILE_PATH));

    verify(googleCloudStorage).get(BUCKET_NAME, FILE_PATH);
  }

  @Test
  public void downloadSourceFileNullTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, gcsUnifiedTemplateDirectory)).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> gcsServiceBean.downloadFile(BUCKET_NAME, gcsUnifiedTemplateDirectory));
    } finally {
      Mockito.verify(googleCloudStorage).get(BUCKET_NAME, gcsUnifiedTemplateDirectory);
    }
  }

  @Test
  public void checkIfBlobExitsTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, gcsUnifiedTemplateDirectory)).thenReturn(blob);
    gcsServiceBean.isFileExists(BUCKET_NAME, gcsUnifiedTemplateDirectory);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, gcsUnifiedTemplateDirectory);
  }

  @Test
  public void checkIfBlobExitsFalseTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, gcsUnifiedTemplateDirectory)).thenReturn(null);
    gcsServiceBean.isFileExists(BUCKET_NAME, gcsUnifiedTemplateDirectory);
    verify(googleCloudStorage).get(BUCKET_NAME, gcsUnifiedTemplateDirectory);
  }

//  @Test
//  public void copyBlobInChunksTest() {
//    Storage.CopyRequest copyRequest = Storage.CopyRequest.newBuilder()
//      .setSource(BlobId.of(BUCKET_NAME, gcsUnifiedTemplateDirectory +Constant.SLASH+ SOURCE_PATH))
//      .setTarget(BlobId.of(BUCKET_NAME, DESTINATION_PATH)).build();
//    CopyWriter copyWriter = Mockito.mock(CopyWriter.class);
//
//
//    Mockito.when(googleCloudStorage.get(BUCKET_NAME,gcsUnifiedTemplateDirectory + Constant.SLASH + SOURCE_PATH)).thenReturn(blob);
//    Mockito.when(googleCloudStorage.copy(Mockito.any(Storage.CopyRequest.class)))
//      .thenReturn(copyWriter);
//    gcsServiceBean.copyBlobInChunks(gcsUnifiedTemplateDirectory + Constant.SLASH + SOURCE_PATH,
//      DESTINATION_PATH);
//    Mockito.verify(googleCloudStorage).get(BUCKET_NAME,
//      gcsUnifiedTemplateDirectory + Constant.SLASH + SOURCE_PATH);
//    Mockito.verify(googleCloudStorage).copy(copyRequest);
//    Mockito.verify(googleCloudStorage)
//      .get(BUCKET_NAME, gcsUnifiedTemplateDirectory + Constant.SLASH + SOURCE_PATH);
//  }

  @Test
  public void testCopyBlobInChunks() throws Exception {
    byte[] content = "test content".getBytes();
    Map<String, String> files = this.getFiles();
    Bucket bucket = mock(Bucket.class);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(googleCloudStorage.get(Mockito.anyString(), Mockito.any())).thenReturn(bucket);
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(excelFile)));
    Blob blob = mock(Blob.class);
    when(googleCloudStorage.get(BUCKET_NAME, SOURCE_PATH)).thenReturn(blob);
    when(blob.getContent()).thenReturn(content);
    when(googleCloudStorage.get(BUCKET_NAME, Storage.BucketGetOption.fields())).thenReturn(bucket);
    when(bucket.create(Mockito.eq(DESTINATION_PATH), Mockito.eq(content), Mockito.eq(contentType))).thenReturn(blob);
    Blob result = gcsServiceBean.copyBlobInChunks(BUCKET_NAME, SOURCE_PATH, DESTINATION_PATH);
    Assertions.assertEquals(blob, result);
    verify(googleCloudStorage).get(BUCKET_NAME, SOURCE_PATH);
    verify(blob).getContent();
    verify(googleCloudStorage).get(BUCKET_NAME, Storage.BucketGetOption.fields());
    verify(bucket).create(Mockito.eq(DESTINATION_PATH), Mockito.eq(content),
      Mockito.eq(contentType));
  }

  @Test
  public void testCopyBlobInChunksNotFound() throws IOException {
    when(googleCloudStorage.get(BUCKET_NAME, SOURCE_PATH)).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> gcsServiceBean.copyBlobInChunks(BUCKET_NAME, SOURCE_PATH, DESTINATION_PATH));
    } finally {
      verify(googleCloudStorage).get(BUCKET_NAME, SOURCE_PATH);
    }
  }

  @Test
  public void uploadCreatedFileStreamTestSuccess() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(excelFile)));
    ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(excelFile);
    when(bucket.create(SOURCE_PATH, byteArrayInputStream, contentType)).thenReturn(blob);
    Blob blobResult =
        gcsServiceBean.uploadCreatedFileStream(bucket, SOURCE_PATH, contentType, byteArrayInputStream);
    Assertions.assertNotNull(blobResult);
    verify(bucket).create(SOURCE_PATH, byteArrayInputStream, contentType);
  }

  @Test
  public void uploadCreatedFileStreamTestFail() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(excelFile)));
    ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(excelFile);
    when(bucket.create(SOURCE_PATH, byteArrayInputStream, contentType)).thenReturn(null);
    Blob blobResult =
        gcsServiceBean.uploadCreatedFileStream(bucket, SOURCE_PATH, contentType, byteArrayInputStream);
    Assertions.assertNull(blobResult);
    verify(bucket).create(SOURCE_PATH, byteArrayInputStream, contentType);
  }

  @Test
  public void deleteFilesByBlobIdsTestEmpty() {
    gcsServiceBean.deleteFilesByBlobIds(null);
  }

  @Test
  public void deleteFilesByBlobIdsTest() {
    Collection<BlobId> blobIds = Collections.singleton(blob.getBlobId());
    when(googleCloudStorage.delete(blobIds)).thenReturn(Collections.singletonList(Boolean.TRUE));
    gcsServiceBean.deleteFilesByBlobIds(blobIds);
    verify(googleCloudStorage).delete(blobIds);
  }

  @Test
  public void listFilesAtDirectoryTest() {
    when(googleCloudStorage.list(BUCKET_NAME,
        Storage.BlobListOption.prefix(DESTINATION_PATH))).thenReturn(null);
    gcsServiceBean.listFilesAtDirectory(BUCKET_NAME, DESTINATION_PATH);
    verify(googleCloudStorage).list(BUCKET_NAME, Storage.BlobListOption.prefix(DESTINATION_PATH));
  }

  @Test
  public void uploadCreatedFileWithMimeTypeTest() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(googleCloudStorage.get(Mockito.anyString(), Mockito.any())).thenReturn(bucket);
    Mockito.when(bucket.create("/temp", excelFile, SOURCE_PATH)).thenReturn(blob);
    Blob blob =
        gcsServiceBean.uploadCreatedFileWithMimeType(bucket, gcsUnifiedTemplateDirectory, excelFile, SOURCE_PATH);
    Assertions.assertNotNull(blob);
  }


  @Test
  public void uploadCreatedFileWithMimeTypeContentTypeNullTest() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(googleCloudStorage.get(Mockito.anyString(), Mockito.any())).thenReturn(bucket);
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(excelFile)));
    Mockito.when(bucket.create("/temp", excelFile, contentType)).thenReturn(blob);
    Blob blob =
        gcsServiceBean.uploadCreatedFileWithMimeType(bucket, gcsUnifiedTemplateDirectory, excelFile, null);
    Assertions.assertNotNull(blob);
  }

  @Test
  public void uploadCreatedFileWithMimeTypeFailedTest() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    Mockito.when(googleCloudStorage.get(Mockito.anyString(), Mockito.any())).thenReturn(bucket);
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(excelFile)));
    Mockito.when(bucket.create("/temp/BP-10001/general_template.xlsm", excelFile, contentType)).thenReturn(null);
    Blob blob =
        gcsServiceBean.uploadCreatedFileWithMimeType(bucket, gcsUnifiedTemplateDirectory, excelFile, contentType);
    Assertions.assertNull(blob);
  }
}
