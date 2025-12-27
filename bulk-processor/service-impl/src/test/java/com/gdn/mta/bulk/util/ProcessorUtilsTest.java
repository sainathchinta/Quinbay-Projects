package com.gdn.mta.bulk.util;


import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.partners.bulk.util.Constant;

public class ProcessorUtilsTest {

  private static final String BULK_RECAT = "Recat";
  private static final String BULK_SAMPLE_FILE = "recat-sample.xlsx";
  private byte[] fileContent = new byte[]{-1, -40, -20, -10};
  private static final String INTERNAL_PROCESS_REQUEST_CODE = "request-code";
  private static final String FILE_NAME = "Copy_product_template.xlsx";
  private static final String DUMMY_PROCESS_TYPE = "DUMMY_PROCESS_TYPE";

  private String filePath;

  @BeforeEach
  public void setUp() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(BULK_RECAT + File.separator + BULK_SAMPLE_FILE).getFile());
    filePath = file.getAbsolutePath();
  }

  @Test
  public void getEarlierDateBySeconds() {
    Assertions.assertTrue(
        ProcessorUtils.getEarlierDateBySeconds(30).getTime() < ProcessorUtils.getEarlierDateBySeconds(0).getTime());
  }

  @Test
  public void testFormatNumberByRemovingDenominator() {
    Assertions.assertEquals("5000", ProcessorUtils.formatNumberByRemovingDenominator(5000.0));
  }

  @Test
  public void testFormatNumberByRemovingDenominator_nullNumber() {
    Assertions.assertNull(ProcessorUtils.formatNumberByRemovingDenominator(null));
  }

  @Test
  public void getExcelSheetDataTest() {
    ProcessorUtils.getExcelSheetData(filePath);
  }

  @Test
  public void getExcelSheetDataExceptionTest() {
    Assertions.assertThrows(RuntimeException.class,
        () -> ProcessorUtils.getExcelSheetData(BULK_RECAT));
  }

  @Test
  public void validateExcelSheet() throws IOException {
    BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest = new BulkInternalProcessUploadRequest();
    bulkInternalProcessUploadRequest.setProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    bulkInternalProcessUploadRequest.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessUploadRequest.setFileName(FILE_NAME);
    mockFile(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + INTERNAL_PROCESS_REQUEST_CODE + Constant.SLASH + FILE_NAME);
    ProcessorUtils.validateExcelFile(bulkInternalProcessUploadRequest);
  }

  @Test
  public void validateExcelSheetStoreCopy() throws IOException {
    BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest = new BulkInternalProcessUploadRequest();
    bulkInternalProcessUploadRequest.setProcessType(BulkInternalProcessType.STORE_COPY.name());
    bulkInternalProcessUploadRequest.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessUploadRequest.setFileName(FILE_NAME);
    mockFile(ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + INTERNAL_PROCESS_REQUEST_CODE + Constant.SLASH + FILE_NAME);
    ProcessorUtils.validateExcelFile(bulkInternalProcessUploadRequest);
  }

  @Test
  public void validateExcelSheetInvalidType() throws IOException {
    BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest = new BulkInternalProcessUploadRequest();
    bulkInternalProcessUploadRequest.setProcessType(StringUtils.EMPTY);
    bulkInternalProcessUploadRequest.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessUploadRequest.setFileName(FILE_NAME);
    ProcessorUtils.validateExcelFile(bulkInternalProcessUploadRequest);
  }

  @Test
  public void validateExcelSheetStoreCopyException() throws IOException {
    BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest = new BulkInternalProcessUploadRequest();
    bulkInternalProcessUploadRequest.setProcessType(BulkInternalProcessType.STORE_COPY.name());
    bulkInternalProcessUploadRequest.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessUploadRequest.setFileName(FILE_NAME);
    Assertions.assertThrows(RuntimeException.class,
        () -> ProcessorUtils.validateExcelFile(bulkInternalProcessUploadRequest));
  }

  @Test
  public void deleteInternalProcessDirectoryByProcessTypeAndRequestCodeTest() {
    ProcessorUtils.deleteInternalProcessDirectoryByProcessTypeAndRequestCode(DUMMY_PROCESS_TYPE,
        INTERNAL_PROCESS_REQUEST_CODE, filePath);
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @AfterEach
  public void tearDown() throws Exception {
    ProcessorUtils.deleteFile(ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS);
    ProcessorUtils.deleteFile(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR);
  }
}
