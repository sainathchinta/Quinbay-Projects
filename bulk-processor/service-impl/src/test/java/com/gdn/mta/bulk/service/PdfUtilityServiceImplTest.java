package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.partners.bulk.util.Constant;
import com.google.cloud.PageImpl;
import com.google.cloud.storage.Blob;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Collections;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PdfUtilityServiceImplTest {
  private static final String GCS_FILE_PATH = "gcs-file-path";
  private static final String OUTPUT_FILE_NAME = "output-file-name.pdf";
  private static final String INPUT_FILE_NAME = "qr-codes.pdf";
  private static final String PATH_PREFIX = "src/test/resources/";
  private static final String TEMP_DIRECTORY = PATH_PREFIX + BulkProcessType.QR_GENERATION.getValue()
      + File.separator + "temp";
  private static final String PERSISTED_DIRECTORY = PATH_PREFIX + BulkProcessType.QR_GENERATION.getValue()
      + File.separator + "persisted";
  @InjectMocks
  private PdfUtilityServiceImpl pdfUtilityService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private Blob blob;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(fileStorageService);
  }

  @Test
  public void generateMergedPdfFileFromPdfFilesAtGcsDirectoryTest() throws Exception {
    Path tempInputPdfPath = Path.of(TEMP_DIRECTORY + Constant.SLASH + INPUT_FILE_NAME);
    Path tempOutputPdfPath = Path.of(TEMP_DIRECTORY + Constant.SLASH + OUTPUT_FILE_NAME);
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + Constant.SLASH + INPUT_FILE_NAME);
    if (!Files.exists(tempInputPdfPath)) {
      Files.createDirectories(tempInputPdfPath);
    }
    Files.copy(persistedPdfPath, tempInputPdfPath, StandardCopyOption.REPLACE_EXISTING);
    Files.deleteIfExists(tempOutputPdfPath);

    try {
      when(blob.getContentType()).thenReturn(MediaType.APPLICATION_PDF_VALUE);
      when(blob.getName()).thenReturn(INPUT_FILE_NAME);
      when(blob.getContent()).thenReturn(Files.readAllBytes(tempInputPdfPath));
      when(fileStorageService.listFilesAtGcsDirectory(GCS_FILE_PATH))
          .thenReturn(new PageImpl<>(null, null, Collections.singleton(blob)));
      pdfUtilityService.generateMergedPdfFileFromPdfFilesAtGcsDirectory(GCS_FILE_PATH,
          TEMP_DIRECTORY, OUTPUT_FILE_NAME);

      Assertions.assertTrue(Files.exists(tempOutputPdfPath));
      verify(fileStorageService).listFilesAtGcsDirectory(GCS_FILE_PATH);
    } finally {
      Files.deleteIfExists(tempInputPdfPath);
      Files.deleteIfExists(tempOutputPdfPath);
      Files.deleteIfExists(Path.of(TEMP_DIRECTORY));
    }
  }

  @Test
  public void generateMergedPdfFileFromPdfFilesAtGcsDirectoryNoInputPDFsTest() throws Exception {
    Path tempInputPdfPath = Path.of(TEMP_DIRECTORY + Constant.SLASH + INPUT_FILE_NAME);
    Path tempOutputPdfPath = Path.of(TEMP_DIRECTORY + Constant.SLASH + OUTPUT_FILE_NAME);
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + Constant.SLASH + INPUT_FILE_NAME);
    if (!Files.exists(tempInputPdfPath)) {
      Files.createDirectories(tempInputPdfPath);
    }
    Files.copy(persistedPdfPath, tempInputPdfPath, StandardCopyOption.REPLACE_EXISTING);
    Files.deleteIfExists(tempOutputPdfPath);

    try {
      when(fileStorageService.listFilesAtGcsDirectory(GCS_FILE_PATH))
          .thenReturn(new PageImpl<>(null, null, Collections.emptyList()));
      pdfUtilityService.generateMergedPdfFileFromPdfFilesAtGcsDirectory(GCS_FILE_PATH,
          TEMP_DIRECTORY, OUTPUT_FILE_NAME);
    } catch (RuntimeException e) {
      Assertions.assertFalse(Files.exists(tempOutputPdfPath));
      verify(fileStorageService).listFilesAtGcsDirectory(GCS_FILE_PATH);
    } finally {
      Files.deleteIfExists(tempInputPdfPath);
      Files.deleteIfExists(tempOutputPdfPath);
      Files.deleteIfExists(Path.of(TEMP_DIRECTORY));
    }
  }

  @Test
  public void generateMergedPdfFileFromPdfFilesAtGcsDirectoryInputNotPDFTest() throws Exception {
    Path tempInputPdfPath = Path.of(TEMP_DIRECTORY + Constant.SLASH + INPUT_FILE_NAME);
    Path tempOutputPdfPath = Path.of(TEMP_DIRECTORY + Constant.SLASH + OUTPUT_FILE_NAME);
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + Constant.SLASH + INPUT_FILE_NAME);
    if (!Files.exists(tempInputPdfPath)) {
      Files.createDirectories(tempInputPdfPath);
    }
    Files.copy(persistedPdfPath, tempInputPdfPath, StandardCopyOption.REPLACE_EXISTING);
    Files.deleteIfExists(tempOutputPdfPath);

    try {
      when(blob.getContentType()).thenReturn(MediaType.TEXT_PLAIN_VALUE);
      when(blob.getName()).thenReturn(INPUT_FILE_NAME);
      when(blob.getContent()).thenReturn(Files.readAllBytes(tempInputPdfPath));
      when(fileStorageService.listFilesAtGcsDirectory(GCS_FILE_PATH))
          .thenReturn(new PageImpl<>(null, null, Collections.singleton(blob)));
      pdfUtilityService.generateMergedPdfFileFromPdfFilesAtGcsDirectory(GCS_FILE_PATH,
          TEMP_DIRECTORY, OUTPUT_FILE_NAME);
    } catch (RuntimeException e) {
      Assertions.assertFalse(Files.exists(tempOutputPdfPath));
      verify(fileStorageService).listFilesAtGcsDirectory(GCS_FILE_PATH);
    } finally {
      Files.deleteIfExists(tempInputPdfPath);
      Files.deleteIfExists(tempOutputPdfPath);
      Files.deleteIfExists(Path.of(TEMP_DIRECTORY));
    }
  }

  @Test
  public void generateMergedPdfFileFromPdfFilesAtGcsDirectoryInputContainsOutputTest() throws Exception {
    Path tempInputPdfPath = Path.of(TEMP_DIRECTORY + Constant.SLASH + INPUT_FILE_NAME);
    Path tempOutputPdfPath = Path.of(TEMP_DIRECTORY + Constant.SLASH + OUTPUT_FILE_NAME);
    Path persistedPdfPath = Path.of(PERSISTED_DIRECTORY + Constant.SLASH + INPUT_FILE_NAME);
    if (!Files.exists(tempInputPdfPath)) {
      Files.createDirectories(tempInputPdfPath);
    }
    Files.copy(persistedPdfPath, tempInputPdfPath, StandardCopyOption.REPLACE_EXISTING);
    Files.deleteIfExists(tempOutputPdfPath);

    try {
      when(fileStorageService.listFilesAtGcsDirectory(GCS_FILE_PATH))
          .thenReturn(new PageImpl<>(null, null, Collections.emptyList()));
      pdfUtilityService.generateMergedPdfFileFromPdfFilesAtGcsDirectory(GCS_FILE_PATH,
          TEMP_DIRECTORY, OUTPUT_FILE_NAME);
    } catch (RuntimeException e) {
      Assertions.assertFalse(Files.exists(tempOutputPdfPath));
      verify(fileStorageService).listFilesAtGcsDirectory(GCS_FILE_PATH);
    } finally {
      Files.deleteIfExists(tempInputPdfPath);
      Files.deleteIfExists(tempOutputPdfPath);
      Files.deleteIfExists(Path.of(TEMP_DIRECTORY));
    }
  }
}
