package com.gdn.mta.bulk.helper;


import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.aspectj.org.eclipse.jdt.core.dom.Modifier;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.InternalBulkProcessFailedData;
import com.gdn.mta.bulk.models.StoreCopyFailedProducts;
import com.gdn.mta.bulk.models.UpdateSalesCategoryFailedProduct;
import com.gdn.mta.bulk.models.download.InternalProcessFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.InternalProcessFailedProductResponse;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.partners.bulk.util.SalesCategoryUpdateConstants;

public class BulkInternalFailedProductsProcessHelperTest {

  private static final String PRODUCT_CODE = "product-code";
  private static final String PRODUCT_NAME = "product-name";

  @InjectMocks
  private BulkInternalFailedProductsProcessHelper bulkInternalFailedProductsProcessHelper;

  @Mock
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  private InternalProcessFailedProductsDownloadRequest internalProcessFailedProductsDownloadRequest;
  private InternalProcessFailedProductResponse internalProcessFailedProductResponse;
  private StoreCopyFailedProducts storeCopyFailedProducts;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    internalProcessFailedProductsDownloadRequest = new InternalProcessFailedProductsDownloadRequest();
    internalProcessFailedProductsDownloadRequest.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS);
    internalProcessFailedProductResponse = new InternalProcessFailedProductResponse();
    internalProcessFailedProductResponse.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS);

    storeCopyFailedProducts =
        StoreCopyFailedProducts.builder().productCode(PRODUCT_CODE).productName(PRODUCT_NAME).build();
    internalProcessFailedProductResponse.setInternalBulkProcessFailedData(Arrays.asList(storeCopyFailedProducts));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkDownloadServiceBeanUtil);
  }

  @Test
  public void testGetCsvHeadersMapTest() {
    bulkInternalFailedProductsProcessHelper.getCsvHeadersMap(internalProcessFailedProductsDownloadRequest);
  }

  @Test
  public void testGetHeaderListTest() {
    bulkInternalFailedProductsProcessHelper.getHeaderList(internalProcessFailedProductResponse);
  }

  @Test
  public void testGetHeaderListSalesCategoryTest() {
    internalProcessFailedProductResponse
        .setBulkProcessEntity(BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS);
    List<String> headers = bulkInternalFailedProductsProcessHelper.getHeaderList(internalProcessFailedProductResponse);
    Assertions.assertTrue(headers.containsAll(SalesCategoryUpdateConstants.TEMPLATE_HEADER));
  }

  @Test
  public void testModifyWorkbookTest() throws Exception {
    bulkInternalFailedProductsProcessHelper.modifyWorkbook(new XSSFWorkbook(), internalProcessFailedProductResponse);
  }

  @Test
  public void getRowDataTest() {
    bulkInternalFailedProductsProcessHelper.getRowData(internalProcessFailedProductResponse);
    Mockito.verify(bulkDownloadServiceBeanUtil)
        .generateStoreCopyRows(Mockito.any(InternalProcessFailedProductResponse.class), Mockito.anyList());
  }

  @Test
  public void getRowDataSalesCategoryTest() {
    internalProcessFailedProductResponse
        .setInternalBulkProcessFailedData(Arrays.asList(new UpdateSalesCategoryFailedProduct()));
    internalProcessFailedProductResponse.setBulkProcessEntity(BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS);
    bulkInternalFailedProductsProcessHelper.getRowData(internalProcessFailedProductResponse);
    Mockito.verify(bulkDownloadServiceBeanUtil)
        .generateUpdateSalesCategoryRows(Mockito.any(InternalProcessFailedProductResponse.class), Mockito.anyList());
  }

  @Test
  public void getRowDataSDefaultTest() {
    internalProcessFailedProductResponse.setBulkProcessEntity(BulkProcessEntity.PRODUCT);
    bulkInternalFailedProductsProcessHelper.getRowData(internalProcessFailedProductResponse);
  }

  @Test
  public void testGetDirectoryTest() {
    bulkInternalFailedProductsProcessHelper.getDirectory(internalProcessFailedProductsDownloadRequest);
  }

  @Test
  public void testGetDirectoryStoreCopy() {
    internalProcessFailedProductsDownloadRequest.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS);
    bulkInternalFailedProductsProcessHelper.getDirectory(internalProcessFailedProductsDownloadRequest);
  }

  @Test
  public void testGetDirectorySalesCategoryCopy() {
    internalProcessFailedProductsDownloadRequest.setBulkProcessEntity(BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS);
    bulkInternalFailedProductsProcessHelper.getDirectory(internalProcessFailedProductsDownloadRequest);
  }

  @Test
  public void testGetEmailParams() {
    bulkInternalFailedProductsProcessHelper.getEmailParams(internalProcessFailedProductsDownloadRequest,
        StringUtils.EMPTY);
  }

  @Test
  public void testGetRecordsUpdatedTest() {
    bulkInternalFailedProductsProcessHelper.getRecordsUpdated(internalProcessFailedProductResponse);
  }
}