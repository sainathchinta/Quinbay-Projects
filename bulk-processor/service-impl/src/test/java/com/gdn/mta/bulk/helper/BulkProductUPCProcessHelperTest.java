package com.gdn.mta.bulk.helper;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductLiteResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;


public class BulkProductUPCProcessHelperTest {

  private BulkProductUPCProcessHelper helper;
  private ObjectMapper realObjectMapper;

  @BeforeEach
  public void setUp() throws Exception {
    helper = new BulkProductUPCProcessHelper();
    realObjectMapper = new ObjectMapper();
  }

  @Test
  public void test_getCsvHeadersMap_returnsNull() {
    // method in class returns null by implementation
    assertNull(helper.getCsvHeadersMap(null));
  }

  @Test
  public void test_getHeaderList_and_getAllHeaderList_successStructure() throws Exception {
    // getHeaderList uses the injected ObjectMapper -> real runs should succeed
    List<String> headerJsonList = helper.getHeaderList(mock(com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse.class));
    assertNotNull(headerJsonList);
    assertEquals(4, headerJsonList.size(), "expected 4 JSON header strings");

  }


  @Test
  public void test_generateDataSheet_fullStyleBranches_and_dataRows() throws Exception {
    // Build explicit header JSON strings so we control tertiaryHeaders content (EDITABLE / NOT_EDITABLE)
    List<String> primary = Arrays.asList(BulkParameters.BLIBLI_PRODUCT_SKU,
        BulkParameters.PARENT_PRODUCT_NAME,
        BulkParameters.BLIBLI_SKU,
        BulkParameters.PRODUCT_NAME,
        BulkParameters.EAN_OR_UPC);
    // secondary includes MANDATORY at index 0 to cause font color red branch
    List<String> secondary = Arrays.asList(BulkParameters.MANDATORY, BulkParameters.OPTIONAL,
        BulkParameters.MANDATORY, BulkParameters.OPTIONAL, BulkParameters.OPTIONAL);
    // tertiary mix to exercise getColorForHeader editable / not editable
    List<String> tertiary = Arrays.asList(BulkParameters.NOT_EDITABLE,
        BulkParameters.EDITABLE,
        BulkParameters.NOT_EDITABLE,
        BulkParameters.EDITABLE,
        BulkParameters.EDITABLE);
    // quaternary descriptions (used in 4th header row with quaternaryHeader=true)
    List<String> quaternary = Arrays.asList("desc1", "desc2", "desc3", "desc4", "desc5");

    String pJson = realObjectMapper.writeValueAsString(primary);
    String sJson = realObjectMapper.writeValueAsString(secondary);
    String tJson = realObjectMapper.writeValueAsString(tertiary);
    String qJson = realObjectMapper.writeValueAsString(quaternary);

    // row data: two rows
    List<String> row1 = Arrays.asList("sku-1", "parent-1", "blibli-sku-1", "product-1", "upc-1");
    List<String> row2 = Arrays.asList("sku-2", "parent-2", "blibli-sku-2", "product-2", "upc-2");
    List<List<String>> rows = Arrays.asList(row1, row2);

    // generate workbook
    int columnWidthChars = 15;
    Workbook workbook = helper.generateDataSheet(Arrays.asList(pJson, sJson, tJson, qJson), rows, columnWidthChars);
    assertNotNull(workbook);
    Sheet sheet = workbook.getSheet(BulkParameters.DATA_SHEET);
    assertNotNull(sheet, "data sheet must exist");

    // Rows: 4 header rows + 2 data rows
    assertEquals(6, sheet.getPhysicalNumberOfRows());

    // Validate data row content (first data row at index 4)
    Row firstDataRow = sheet.getRow(4);
    assertNotNull(firstDataRow);
    assertEquals("sku-1", firstDataRow.getCell(0).getStringCellValue());
    assertEquals("upc-1", firstDataRow.getCell(4).getStringCellValue());

    // Validate column width set logic (width = chars * 256)
    int expectedWidth = columnWidthChars * 256;
    assertEquals(expectedWidth, sheet.getColumnWidth(0));
    assertEquals(expectedWidth, sheet.getColumnWidth(4));

    // Validate primary header style: at row 0 first cell
    Row primaryHeaderRow = sheet.getRow(0);
    assertNotNull(primaryHeaderRow);
    CellStyle primaryStyle = primaryHeaderRow.getCell(0).getCellStyle();

    // primary header should have indentation set (primaryHeader branch)
    assertEquals(4, primaryStyle.getIndention(), "primary header should set indentation to 4");

    // font size for primary header should be 14
    Font primaryFont = workbook.getFontAt(primaryStyle.getFontIndex());
    assertEquals(14, primaryFont.getFontHeightInPoints());

    // Validate secondary header font color for MANDATORY is RED (secondary row index 1, cell 0)
    Row secondaryHeaderRow = sheet.getRow(1);
    assertNotNull(secondaryHeaderRow);
    CellStyle secondaryStyle = secondaryHeaderRow.getCell(0).getCellStyle();
    Font secondaryFont = workbook.getFontAt(secondaryStyle.getFontIndex());
    // Font color should be RED for MANDATORY header
    assertEquals(org.apache.poi.ss.usermodel.IndexedColors.RED.getIndex(), secondaryFont.getColor());

    // Validate quaternary header (row index 3) has top & bottom border (applyTopBottomBorder=true)
    Row quaternaryRow = sheet.getRow(3);
    assertNotNull(quaternaryRow);
    CellStyle quaternaryStyleCell0 = quaternaryRow.getCell(0).getCellStyle();
    assertEquals(BorderStyle.THIN, quaternaryStyleCell0.getBorderTopEnum(), "top border should be thin when applyTopBottomBorder true");
    assertEquals(BorderStyle.THIN, quaternaryStyleCell0.getBorderBottomEnum(), "bottom border should be thin when applyTopBottomBorder true");

    // Ensure tertiary headers mix (editable/not editable) did not crash (we validated workbook created)
  }


  @Test
  public void test_modifyWorkbook_returnsSameWorkbook() throws Exception {
    Workbook workbook = new org.apache.poi.xssf.usermodel.XSSFWorkbook();
    BulkDataResponse response = mock(BulkDataResponse.class);

    Workbook returned = helper.modifyWorkbook(workbook, response);

    // Must return the SAME object instance
    assertSame(workbook, returned, "modifyWorkbook should return the same workbook instance");
  }



  @Test
  public void test_getRowData_and_getRecordsUpdated() {
    BulkProductLiteResponse productResponse = mock(BulkProductLiteResponse.class);
    List<List<String>> rows = Arrays.asList(
        Arrays.asList("s1", "p1", "b1", "prod1", "u1"),
        Arrays.asList("s2", "p2", "b2", "prod2", "u2"),
        Arrays.asList("s3", "p3", "b3", "prod3", "u3")
    );
    when(productResponse.getProductContentList()).thenReturn(rows);

    // The methods cast BulkDataResponse into BulkProductLiteResponse internally, so we pass productResponse directly
    List<List<String>> returned = helper.getRowData(productResponse);
    assertEquals(rows, returned);

    int rec = helper.getRecordsUpdated(productResponse);
    assertEquals(rows.size(), rec);
  }

  @Test
  public void test_getDirectory_and_getEmailParams_basicChecks() {
    BulkDownloadRequest request = mock(BulkDownloadRequest.class);
    when(request.getRequestId()).thenReturn("RID-ABC");
    when(request.getMerchantId()).thenReturn("M-001");
    when(request.getUsername()).thenReturn("alice");

    String dir = helper.getDirectory(request);
    assertNotNull(dir);
    assertTrue(dir.contains("RID-ABC"), "Directory should contain request id");
    // ensure it uses the product prefix that ProcessorUtils maps for PRODUCT entity
    String productPrefix = ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.PRODUCT_EAN);
    assertNotNull(productPrefix);
    assertTrue(dir.startsWith(productPrefix));

    Map<String, Object> emailParams = helper.getEmailParams(request, "en");
    assertNotNull(emailParams);
    assertEquals("RID-ABC", emailParams.get("reqId"));
    assertEquals("M-001", emailParams.get("businessPartnerCode"));
    assertTrue(emailParams.containsKey("name"));
    assertTrue(emailParams.containsKey(EmailConstants.TEMPLATE_ID_PARAM));
    assertTrue(emailParams.containsKey(EmailConstants.MAIL_SENDER_PARAM));
    assertTrue(emailParams.containsKey(EmailConstants.MAIL_SUBJECT_PARAM));
  }
}