package com.gdn.mta.bulk.helper;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.dto.TaggedProductFilterResponse;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.response.BulkDownloadTaggedProductsResponse;

public class BulkDownloadTaggedProductsHelperTest {

  @InjectMocks
  private BulkDownloadTaggedProductsHelper bulkDownloadTaggedProductsHelper;


  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testGetHeaderList() throws Exception {
    BulkDataResponse response = new BulkDataResponse();
    List<String> headers = bulkDownloadTaggedProductsHelper.getHeaderList(response);
    Assertions.assertNotNull(headers);
    Assertions.assertEquals("SKU Id", headers.get(0));
  }

  @Test
  public void testGenerateDataSheet() {
    List<String> headers = Arrays.asList("SKU", "PP Code", "Product Type");
    List<List<String>> rows =
        Arrays.asList(Arrays.asList("SKU1", "PP1", "Type1"), Arrays.asList("SKU2", "PP2", "Type2"));
    Workbook workbook = bulkDownloadTaggedProductsHelper.generateDataSheet(headers, rows, 1);
    Assertions.assertNotNull(workbook);
    Assertions.assertEquals("Data Sheet", workbook.getSheetName(0));
    Assertions.assertEquals("SKU", workbook.getSheetAt(0).getRow(0).getCell(0).getStringCellValue());
    Assertions.assertEquals("SKU1", workbook.getSheetAt(0).getRow(1).getCell(0).getStringCellValue());
  }

  @Test
  public void modifyWorkbookTest() throws Exception {
    Workbook workbook = bulkDownloadTaggedProductsHelper.modifyWorkbook(null, new BulkDataResponse());
    Assertions.assertNull(workbook);
  }

  @Test
  public void getRowDataTest(){
    BulkDownloadTaggedProductsResponse mockResponse = new BulkDownloadTaggedProductsResponse();
    List<TaggedProductFilterResponse> mockFilterResponseList = new ArrayList<>();
    TaggedProductFilterResponse mockFilterResponse = new TaggedProductFilterResponse();
    mockFilterResponse.setItemSku("SKU123");
    mockFilterResponse.setPickupPointCode("Point123");
    mockFilterResponse.setParentCategoryNames(List.of("Category1", "Category2"));
    mockFilterResponse.setBusinessPartnerName("Partner123");
    mockFilterResponse.setProductTypeName("Type123");
    mockFilterResponse.setUserUpdatedDate(new Date());
    TaggedProductFilterResponse mockFilterResponse1 = new TaggedProductFilterResponse();
    mockFilterResponse1.setItemSku("SKU123");
    mockFilterResponse1.setPickupPointCode("Point123");
    mockFilterResponse1.setParentCategoryNames(null);
    mockFilterResponse1.setBusinessPartnerName("Partner123");
    mockFilterResponse1.setProductTypeName("Type123");
    mockFilterResponse1.setUserUpdatedDate(null);
    mockFilterResponseList.add(mockFilterResponse);
    mockFilterResponseList.add(mockFilterResponse1);
    mockResponse.setTaggedProductFilterResponseList(mockFilterResponseList);
    List<List<String>> rowData = bulkDownloadTaggedProductsHelper.getRowData(mockResponse);
    Assertions.assertEquals(2, rowData.size());
    List<String> row = rowData.get(0);
    List<String> row1 = rowData.get(1);
    Assertions.assertEquals(6, row.size());
    Assertions.assertEquals("SKU123", row.get(0));
    Assertions.assertEquals("Point123", row.get(1));
    Assertions.assertEquals("Category1 > Category2", row.get(2));
    Assertions.assertEquals("Partner123", row.get(3));
    Assertions.assertEquals("Type123", row.get(4));
    Assertions.assertEquals("SKU123", row1.get(0));
    Assertions.assertEquals("Point123", row1.get(1));
    Assertions.assertEquals(StringUtils.EMPTY, row1.get(2));
    Assertions.assertEquals("Partner123", row1.get(3));
    Assertions.assertEquals("Type123", row1.get(4));
  }

  @Test
  public void getDirectoryTest(){
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setRequestId("requestId");
    String path = bulkDownloadTaggedProductsHelper.getDirectory(bulkDownloadRequest);
    Assertions.assertEquals("target/x-bulk/taggedProducts/requestId",path);
  }

  @Test
  public void getEmailParamsTest() {
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setRequestId("requestId");
    Map<String, Object> emailParameters = bulkDownloadTaggedProductsHelper.getEmailParams(bulkDownloadRequest, "ENG");
    Assertions.assertEquals(EmailConstants.BULK_TAGGING_PRODUCTS_DOWNLOAD_TEMPLATE_ID,
        emailParameters.get(EmailConstants.TEMPLATE_ID_PARAM));
  }

  @Test
  public void getRecordsUpdatedTest(){
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    int recordsUpdated = bulkDownloadTaggedProductsHelper.getRecordsUpdated(bulkDataResponse);
    Assertions.assertEquals(0,recordsUpdated);
  }
}
