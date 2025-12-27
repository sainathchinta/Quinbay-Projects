package com.gdn.mta.bulk.service.download;

import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.BulkInternalProcessDataPayload;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.SalesCategoryUpdateRequest;
import com.gdn.mta.bulk.models.download.InternalProcessFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.InternalProcessFailedProductResponse;
import com.gdn.mta.bulk.service.InternalProcessService;
import com.gdn.partners.bulk.util.Constant;


public class BulkInternalFailedProductsServiceBeanTest {

  private static final String PRODUCT_CODE = "product-code";
  public static final String REQUEST_CODE = "request-code";

  @InjectMocks
  private BulkInternalFailedProductsServiceBean bulkInternalFailedProductsServiceBean;

  @Mock
  private InternalProcessService internalProcessService;

  @Mock
  private ObjectMapper objectMapper;

  private InternalProcessFailedProductsDownloadRequest internalProcessFailedProductsDownloadRequest;
  private InternalProcessFailedProductResponse internalProcessFailedProductResponse;
  private BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
  private BulkInternalProcessDataPayload bulkInternalProcessDataPayload;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    internalProcessFailedProductsDownloadRequest = new InternalProcessFailedProductsDownloadRequest();
    internalProcessFailedProductsDownloadRequest.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS);
    internalProcessFailedProductsDownloadRequest.setInternalProcessRequestCode(REQUEST_CODE);
    internalProcessFailedProductResponse = new InternalProcessFailedProductResponse();
    internalProcessFailedProductResponse.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS);

    bulkInternalProcessData.setStatus(ProcessStatus.PENDING.name());
    bulkInternalProcessData.setParentCode(PRODUCT_CODE);

    bulkInternalProcessDataPayload = BulkInternalProcessDataPayload.builder().productCode(PRODUCT_CODE).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
  }

  @Test
  public void testGetData() throws Exception {
    Mockito.when(internalProcessService.getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(
        Constant.STORE_ID, internalProcessFailedProductsDownloadRequest.getInternalProcessRequestCode(),
        ProcessStatus.FAILED.name())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), BulkInternalProcessDataPayload.class))
        .thenReturn(bulkInternalProcessDataPayload);
    bulkInternalFailedProductsServiceBean.getData(internalProcessFailedProductsDownloadRequest);
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(Constant.STORE_ID,
            internalProcessFailedProductsDownloadRequest.getInternalProcessRequestCode(), ProcessStatus.FAILED.name());
    Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), BulkInternalProcessDataPayload.class);
  }

  @Test
  public void testGetDataSalesCategory() throws Exception {
    internalProcessFailedProductsDownloadRequest.setBulkProcessEntity(BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS);
    Mockito.when(internalProcessService.getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(
        Constant.STORE_ID, internalProcessFailedProductsDownloadRequest.getInternalProcessRequestCode(),
        ProcessStatus.FAILED.name())).thenReturn(Arrays.asList(bulkInternalProcessData));
    Mockito.when(objectMapper.readValue(bulkInternalProcessData.getData(), SalesCategoryUpdateRequest.class))
        .thenReturn(new SalesCategoryUpdateRequest());
    bulkInternalFailedProductsServiceBean.getData(internalProcessFailedProductsDownloadRequest);
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(Constant.STORE_ID,
            internalProcessFailedProductsDownloadRequest.getInternalProcessRequestCode(), ProcessStatus.FAILED.name());
    Mockito.verify(objectMapper).readValue(bulkInternalProcessData.getData(), SalesCategoryUpdateRequest.class);
  }

  @Test
  public void testGetDataDefault() throws Exception {
    internalProcessFailedProductsDownloadRequest.setBulkProcessEntity(BulkProcessEntity.PRODUCT);
    Mockito.when(internalProcessService.getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(
        Constant.STORE_ID, internalProcessFailedProductsDownloadRequest.getInternalProcessRequestCode(),
        ProcessStatus.FAILED.name())).thenReturn(Arrays.asList(bulkInternalProcessData));
    bulkInternalFailedProductsServiceBean.getData(internalProcessFailedProductsDownloadRequest);
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(Constant.STORE_ID,
            internalProcessFailedProductsDownloadRequest.getInternalProcessRequestCode(), ProcessStatus.FAILED.name());
  }


  @Test
  public void testGetDataNoFailed() throws Exception {
    Mockito.when(internalProcessService.getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(
        Constant.STORE_ID, internalProcessFailedProductsDownloadRequest.getInternalProcessRequestCode(),
        ProcessStatus.FAILED.name())).thenReturn(Arrays.asList());
    bulkInternalFailedProductsServiceBean.getData(internalProcessFailedProductsDownloadRequest);
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(Constant.STORE_ID,
            internalProcessFailedProductsDownloadRequest.getInternalProcessRequestCode(), ProcessStatus.FAILED.name());
  }
}