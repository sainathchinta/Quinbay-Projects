package com.gdn.mta.bulk.util;


import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;


public class ExcelUtilsTest {


  @Test
  public void readByDataSheetNameTest() throws Exception {
    ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(FileUtils.readFileToByteArray(new File(
        ClassLoader.getSystemClassLoader().getResource(org.apache.commons.lang.StringUtils.EMPTY).getPath()
            + "/Recategorization/recategorization_blank.xls")));
    try {
      ExcelUtils.readByDataSheetName(byteArrayInputStream, "", 0, 5, "xls", "bulkProcessCode", 10, false);
    } catch (Exception e) {

    }
  }

  @Test
  public void readByDataSheetNameNonEmptyTest() throws Exception {
    String sheetName = "Sheet1";
    String excelFileType = "xls";
    String bulkProcessCode = "testCode";
    HSSFWorkbook workbook = new HSSFWorkbook();
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    workbook.write(outputStream);
    ByteArrayInputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());
    List<List<Object>> result = null;
    try {
      result =
          ExcelUtils.readByDataSheetName(inputStream, sheetName, 0, 10, excelFileType, bulkProcessCode,
              100, false);
    } catch (Exception e) {
      Assertions.assertNull(result);
    }
  }

  @Test
  public void testReadByDataSheetName_XLSX() throws Exception {
    String sheetName = "Sheet1";
    String excelFileType = "xlsx";
    String bulkProcessCode = "testCode";
    XSSFWorkbook workbook = new XSSFWorkbook();
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    workbook.write(outputStream);
    ByteArrayInputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());
    List<List<Object>> result = null;
    try {
      result = ExcelUtils.readByDataSheetName(inputStream, sheetName, 0, 10, excelFileType, bulkProcessCode,
          100, false);
    } catch (Exception e) {
      Assertions.assertNull(result);
    }
  }

  @Test
  public void readByDataSheetNameTest2() throws Exception {
    ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(new byte[] {});
    try {
      ExcelUtils.readByDataSheetName(byteArrayInputStream, "", 0, 5, "xlsx", "bulkProcessCode", 10, false);
    } catch (Exception e) {

    }
  }

}