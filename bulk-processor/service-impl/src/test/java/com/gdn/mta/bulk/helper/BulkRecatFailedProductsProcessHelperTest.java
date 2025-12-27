package com.gdn.mta.bulk.helper;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.RecatFailedProducts;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.RecatFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.RecatFailedProductResponse;

public class BulkRecatFailedProductsProcessHelperTest {

  public static final String PRODUCT_NAME = "product-name";
  public static final String PRODUCT_CODE = "product-code";
  public static final String ERROR_MSG = "error-msg";
  public static final String MASTER_CATEGORY_CODE = "MasterCategoryCode";
  public static final String MASTER_CATEGORY_NAME = "MasterCategoryName";
  public static final String NEW_MASTER_CATEGORY_CODE = "NewMasterCategoryCode";
  public static final String NEW_MASTER_CATEGORY_NAME = "NewMasterCategoryName";

  @InjectMocks
  BulkRecatFailedProductsProcessHelper bulkRecatFailedProductsProcessHelper;

  private RecatFailedProductResponse recatFailedProductResponse;

  private List<RecatFailedProducts> recatFailedProductsList;

  private RecatFailedProductsDownloadRequest bulkDownloadRequest;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    recatFailedProductResponse = new RecatFailedProductResponse();
    recatFailedProductResponse.setBulkProcessEntity(BulkProcessEntity.RECAT_FAILED_PRODUCTS);
    RecatFailedProducts recatFailedProducts = new RecatFailedProducts();
    recatFailedProducts.setProductName(PRODUCT_NAME);
    recatFailedProducts.setProductCode(PRODUCT_CODE);
    recatFailedProducts.setErrorMessage(ERROR_MSG);
    recatFailedProducts.setMasterCategoryCode(MASTER_CATEGORY_CODE);
    recatFailedProducts.setMasterCategoryName(MASTER_CATEGORY_NAME);
    recatFailedProducts.setNewMasterCategoryCode(NEW_MASTER_CATEGORY_CODE);
    recatFailedProducts.setMasterCategoryName(NEW_MASTER_CATEGORY_NAME);
    recatFailedProductsList = new ArrayList<>();
    recatFailedProductsList.add(recatFailedProducts);
    recatFailedProductResponse.setResponseList(recatFailedProductsList);
    bulkDownloadRequest = new RecatFailedProductsDownloadRequest();
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.RECAT_FAILED_PRODUCTS);
  }

  @Test
  public void getCsvHeadersMapTest() throws Exception {
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    BulkCsvModel bulkCsvModel = this.bulkRecatFailedProductsProcessHelper.getCsvHeadersMap(bulkDownloadRequest);
    Assertions.assertNull(bulkCsvModel);
  }

  @Test
  public void generateCsvFileTest() throws  Exception {
    BulkCsvModel bulkCsvModels = new BulkCsvModel(new ArrayList<>(), new HashMap<>());
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    byte[] bulkCsvModel = this.bulkRecatFailedProductsProcessHelper.generateCsvFile(bulkCsvModels, bulkDataResponse);
    assertNotNull(bulkCsvModel);
  }

  @Test
  public void getHeaderListTest() throws  Exception {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    List<String> response = this.bulkRecatFailedProductsProcessHelper.getHeaderList(bulkDataResponse);
    assertNotNull(response);
  }

  @Test
  @Disabled
  public void generateDataSheetTest() throws  Exception {
    List<String> headers = new ArrayList<>();
    List<List<String>> rows = new ArrayList<>();
    Workbook response = this.bulkRecatFailedProductsProcessHelper.generateDataSheet(headers, rows, 1);
    assertNotNull(response);
  }

  @Test
  public void getUserNameTest() throws  Exception {
    String response = this.bulkRecatFailedProductsProcessHelper.getUserName("target");
    assertNotNull(response);
  }

  @Test
  public void modifyWorkbookTest() throws Exception {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    Workbook workbook = mock(Workbook.class);
    Workbook response = this.bulkRecatFailedProductsProcessHelper.modifyWorkbook(workbook, bulkDataResponse);
  }

  @Test
  public void getRowDataTest() throws Exception {
    bulkRecatFailedProductsProcessHelper.getRowData(recatFailedProductResponse);
  }

  @Test
  public void getDirectoryTest() throws Exception {
    bulkRecatFailedProductsProcessHelper.getDirectory(bulkDownloadRequest);
  }

  @Test
  public void getFilePathTest() throws Exception {
    String file = this.bulkRecatFailedProductsProcessHelper.getFilePath("FOO", "BAR");
    assertNotNull(file);
  }

  @Test
  public void getEmailParamsTest() throws Exception {
    Map<String, Object> file = this.bulkRecatFailedProductsProcessHelper.getEmailParams(bulkDownloadRequest, "in");
    Assertions.assertNull(file);
  }

  @Test
  public void getRecordsUpdated() throws Exception {
    int records = this.bulkRecatFailedProductsProcessHelper.getRecordsUpdated(recatFailedProductResponse);
    Assertions.assertEquals(records, 1);
  }

}
