package com.gdn.partners.product.analytics.service.impl.helper;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.zip.GZIPOutputStream;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Storage;
import com.google.cloud.storage.StorageBatch;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.product.analytics.model.enums.FileExtensions;
import com.gdn.partners.product.analytics.properties.FileHelperProperties;
import com.gdn.partners.product.analytics.service.impl.exception.FileException;

public class FileHelperImplTest {

  @Mock
  private FileHelperProperties fileHelperProperties;

  @Mock
  private Storage imageGoogleCloudStorage;

  @InjectMocks
  private FileHelperImpl fileHelper;

  private static final String RESULT_FILE_NAME_LOCAL = "result";
  private static final String FILE_EXTENSION = FileExtensions.NDJSON.getExtension();
  private static final int NO_OF_RECORDS_PER_FILE = 2;
  private static final String LINE_1 = "line1";
  private static final String LINE_2 = "line2";
  private static final String LINE_3 = "line3";
  private static final String JSON_FILE = "jsonFile";
  private static File actualUncompressedFile;
  private static File compressedFile;
  private static File compressedFile2;
  private static File jsonFile;
  private static File unSplitFile;
  private static final String SAMPLE_CONTENT = "sample content for an uncompressed file";
  private static final String BUCKET_NAME = "bucketName";
  private static final String FOLDER_PATH = "/folderPath";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(fileHelperProperties.getBufferSize()).thenReturn(4);
    when(fileHelperProperties.getDirectory()).thenReturn(StringUtils.EMPTY);
    compressedFile = new File(RESULT_FILE_NAME_LOCAL + FileExtensions.NDJSON.getExtension() +
        FileExtensions.GZIP.getExtension());
    compressedFile2 = new File(RESULT_FILE_NAME_LOCAL + FileExtensions.NDJSON.getExtension() +
      FileExtensions.GZIP.getExtension());
    actualUncompressedFile = new File(RESULT_FILE_NAME_LOCAL + FileExtensions.NDJSON.getExtension());
    jsonFile = new File(JSON_FILE);
    unSplitFile = new File(RESULT_FILE_NAME_LOCAL + FileExtensions.NDJSON.getExtension());
  }

  @AfterEach
  public void tearDown() throws Exception {
    compressedFile.delete();
    actualUncompressedFile.delete();
    jsonFile.delete();
    unSplitFile.delete();
    Mockito.verifyNoMoreInteractions(fileHelperProperties);
  }

  @Test
  public void splitFileIntoSmallFiles() throws Exception{
    try(BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(unSplitFile))) {
      bufferedWriter.write(LINE_1);
      bufferedWriter.newLine();
      bufferedWriter.write(LINE_2);
      bufferedWriter.newLine();
      bufferedWriter.write(LINE_3);
    }
    try(BufferedReader originalReader = new BufferedReader(new FileReader(unSplitFile))){
      List<String> splitFiles =
          fileHelper.splitFileIntoSmallFiles(RESULT_FILE_NAME_LOCAL, FILE_EXTENSION,
            NO_OF_RECORDS_PER_FILE, List.of(RESULT_FILE_NAME_LOCAL + FILE_EXTENSION));
      Assertions.assertEquals(2, splitFiles.size());
      verify(fileHelperProperties, times(2)).getDirectory();
      for (String splitFilePath: splitFiles) {
        try(BufferedReader reader = new BufferedReader(new FileReader(splitFilePath))){
          String line = reader.readLine();
          while (Objects.nonNull(line)){
            Assertions.assertEquals(originalReader.readLine(), line);
            line = reader.readLine();
          }
        }
        File file = new File(splitFilePath);
        file.delete();
      }
    }
  }
  @Test
  public void unzipFileExceptionTest2() throws IOException {
    mockFiles();
    compressExpectedUncompressedFile();
    fileHelper.unZipFile(RESULT_FILE_NAME_LOCAL, StringUtils.EMPTY, new ArrayList<>());
    List<String> fileList = fileHelper.unZipFile(RESULT_FILE_NAME_LOCAL, StringUtils.EMPTY, new ArrayList<>());
    Mockito.verify(fileHelperProperties).getBufferSize();
    Assertions.assertEquals(0, fileList.size());
  }

  @Test
  public void splitFileIntoSmallFilesExceptionTest() throws Exception {
    Mockito.when(fileHelperProperties.getDirectory()).thenReturn("nonexistent/directory");
    try (BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(unSplitFile))) {
      bufferedWriter.write(LINE_1);
      bufferedWriter.newLine();
      bufferedWriter.write(LINE_2);
      bufferedWriter.newLine();
      bufferedWriter.write(LINE_3);
    }
      List<String> splitFiles = new ArrayList<>();
      try {
        splitFiles =
            fileHelper.splitFileIntoSmallFiles(StringUtils.EMPTY, FILE_EXTENSION,
              NO_OF_RECORDS_PER_FILE, Collections.singletonList(RESULT_FILE_NAME_LOCAL+FILE_EXTENSION));
      } catch (FileException e) {
      } finally {
        verify(fileHelperProperties, times(1)).getDirectory();
        for (String splitFilePath : splitFiles) {
          try (BufferedReader reader = new BufferedReader(new FileReader(splitFilePath))) {
            String line = reader.readLine();
            while (Objects.nonNull(line)) {
              line = reader.readLine();
            }
          }
          File file = new File(splitFilePath);
          file.delete();
        }
      }
  }



  @Test
  public void unzipFile() throws IOException {
    mockFiles();
    compressExpectedUncompressedFile();
    List<String> unZipFile =
      fileHelper.unZipFile(RESULT_FILE_NAME_LOCAL, FileExtensions.NDJSON.getExtension(),
        List.of(compressedFile.getAbsolutePath(), compressedFile2.getAbsolutePath()));
    Assertions.assertEquals(2, unZipFile.size());
    Mockito.verify(fileHelperProperties, Mockito.times(3)).getBufferSize();
    Mockito.verify(fileHelperProperties, Mockito.times(2)).getDirectory();
  }

  @Test
  public void deleteFilesFromDirectory() throws IOException {
    File file = new File("test");
    file.mkdirs();
    Mockito.when(fileHelperProperties.getDirectory()).thenReturn("test");
    fileHelper.deleteFilesFromAutoQCDirectory();
    Mockito.verify(fileHelperProperties, Mockito.times(1)).getDirectory();
    FileUtils.deleteDirectory(file);
  }

  @Test
  public void deleteFilesExceptionFromDirectory() throws IOException {
    Mockito.when(fileHelperProperties.getDirectory()).thenThrow(RuntimeException.class);
    try {
      fileHelper.deleteFilesFromAutoQCDirectory();
    } catch (RuntimeException e) {
    } finally {
      Mockito.verify(fileHelperProperties, Mockito.times(1)).getDirectory();
    }
  }

  @Test
  public void unzipFileExceptionTest() {
    when(fileHelperProperties.getBufferSize()).thenReturn(-1);
    try {
      mockFiles();
      compressExpectedUncompressedFile();
        fileHelper.unZipFile(RESULT_FILE_NAME_LOCAL, FileExtensions.NDJSON.getExtension(),
          Collections.emptyList());
    } catch (Exception e) {
      Assertions.assertNotNull(e.getMessage());
    } finally {
      Mockito.verify(fileHelperProperties, Mockito.times(1)).getBufferSize();
    }
  }

  @Test
  public void deleteDirectoryFromGcsTest() {
    mockGoogleCloudStorage();
    when(fileHelperProperties.getBatchSizeForImageDeletion()).thenReturn(2);
    int actualValue = fileHelper.deleteDirectoryFromGcs(BUCKET_NAME, FOLDER_PATH);
    Mockito.verify(fileHelperProperties, Mockito.times(3)).getBatchSizeForImageDeletion();
    Assertions.assertEquals(3, actualValue);
  }

  @Test
  public void deleteDirectoryFromGcsTest_equalBatchSize() {
    mockGoogleCloudStorage();
    when(fileHelperProperties.getBatchSizeForImageDeletion()).thenReturn(3);
    int actualValue = fileHelper.deleteDirectoryFromGcs(BUCKET_NAME, FOLDER_PATH);
    Mockito.verify(fileHelperProperties, Mockito.times(3)).getBatchSizeForImageDeletion();
    Assertions.assertEquals(3, actualValue);
  }

  @Test
  public void deleteDirectoryFromGcsTest_exception() {
    com.google.api.gax.paging.Page<Blob> blobPage = mock(com.google.api.gax.paging.Page.class);
    Blob blob = mock(Blob.class);
    String csv = "hi, hello";
    when(blob.getContent()).thenReturn(csv.getBytes());
    when(blobPage.getValues()).thenReturn(Collections.singletonList(blob));
    when(imageGoogleCloudStorage.batch()).thenReturn(Mockito.mock(StorageBatch.class));
    when(imageGoogleCloudStorage.list(eq(BUCKET_NAME), any(Storage.BlobListOption.class))).thenReturn(blobPage);
    when(fileHelperProperties.getBatchSizeForImageDeletion()).thenThrow(new ApplicationRuntimeException());
    int actualValue = fileHelper.deleteDirectoryFromGcs(BUCKET_NAME, FOLDER_PATH);
    Mockito.verify(fileHelperProperties).getBatchSizeForImageDeletion();
    Assertions.assertEquals(1, actualValue);
  }

  private void mockGoogleCloudStorage(){
    com.google.api.gax.paging.Page<Blob> blobPage = mock(com.google.api.gax.paging.Page.class);
    Blob blob = mock(Blob.class);
    Blob blob2 = mock(Blob.class);
    Blob blob3 = mock(Blob.class);
    String csv = "hi, hello";
    when(blob.getContent()).thenReturn(csv.getBytes());
    when(blob2.getContent()).thenReturn(csv.getBytes());
    when(blob3.getContent()).thenReturn(csv.getBytes());
    when(blobPage.getValues()).thenReturn(Arrays.asList(blob, blob2, blob3));
    when(imageGoogleCloudStorage.list(eq(BUCKET_NAME), any(Storage.BlobListOption.class))).thenReturn(blobPage);
    when(imageGoogleCloudStorage.batch()).thenReturn(Mockito.mock(StorageBatch.class));
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
}
