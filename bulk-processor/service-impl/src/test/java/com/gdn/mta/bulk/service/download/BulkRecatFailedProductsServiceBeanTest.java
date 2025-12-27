package com.gdn.mta.bulk.service.download;

import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.mta.bulk.entity.ProductRecatStatus;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.RecatFailedProducts;
import com.gdn.mta.bulk.models.download.RecatFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.RecatFailedProductResponse;
import com.gdn.mta.bulk.repository.ProductRecatStatusRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.RecatConstants;

public class BulkRecatFailedProductsServiceBeanTest {

  public static final String RECAT_REQUEST_CODE = "recat-cat-code";
  public static final String PRODUCT_NAME = "product-name";
  public static final String PRODUCT_CODE = "product-code";
  public static final String ERROR_MSG = "error-msg";

  @InjectMocks
  BulkRecatFailedProductsServiceBean bulkRecatFailedProductsServiceBean;

  @Mock
  ProductRecatStatusRepository productRecatStatusRepository;

  private RecatFailedProductResponse recatFailedProductResponse;

  private List<RecatFailedProducts> recatFailedProductsList;

  private RecatFailedProductsDownloadRequest bulkDownloadRequest;

  private List<ProductRecatStatus> productRecatStatusList;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    recatFailedProductResponse = new RecatFailedProductResponse();
    recatFailedProductResponse.setBulkProcessEntity(BulkProcessEntity.RECAT_FAILED_PRODUCTS);
    RecatFailedProducts recatFailedProducts = new RecatFailedProducts();
    recatFailedProducts.setProductName(PRODUCT_NAME);
    recatFailedProducts.setProductCode(PRODUCT_CODE);
    recatFailedProducts.setErrorMessage(ERROR_MSG);
    RecatFailedProducts recatFailedProducts1 = new RecatFailedProducts();
    recatFailedProducts1.setProductName(PRODUCT_NAME);
    recatFailedProducts1.setProductCode(PRODUCT_CODE);
    recatFailedProducts1.setErrorMessage("");
    recatFailedProductsList = new ArrayList<>();
    recatFailedProductsList.add(recatFailedProducts);
    recatFailedProductsList.add(recatFailedProducts1);
    recatFailedProductResponse.setResponseList(recatFailedProductsList);
    bulkDownloadRequest = new RecatFailedProductsDownloadRequest();
    bulkDownloadRequest.setRecatRequestCode(RECAT_REQUEST_CODE);
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.RECAT_FAILED_PRODUCTS);
    productRecatStatusList = new ArrayList<>();
    ProductRecatStatus productRecatStatus = new ProductRecatStatus();
    productRecatStatus.setProductCode(PRODUCT_CODE);
    productRecatStatus.setProductName(PRODUCT_CODE);
    productRecatStatus.setErrorMessage(ERROR_MSG);
    productRecatStatus.setValidationError(true);
    ProductRecatStatus productRecatStatus1 = new ProductRecatStatus();
    productRecatStatus1.setProductCode(PRODUCT_CODE);
    productRecatStatus1.setProductName(PRODUCT_CODE);
    productRecatStatus1.setErrorMessage(ERROR_MSG);
    productRecatStatus1.setValidationError(false);
    productRecatStatusList.add(productRecatStatus);
    productRecatStatusList.add(productRecatStatus1);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.productRecatStatusRepository);
  }

  @Test
  public void getDataTest() throws Exception{
    Mockito.when(productRecatStatusRepository.findByStoreIdAndStatusAndRecatRequestCode(Constant.STORE_ID,
        RecatConstants.FAILED, RECAT_REQUEST_CODE))
        .thenReturn(productRecatStatusList);
    bulkRecatFailedProductsServiceBean.getData(bulkDownloadRequest);
    Mockito.verify(productRecatStatusRepository).findByStoreIdAndStatusAndRecatRequestCode(Constant.STORE_ID,
        RecatConstants.FAILED, RECAT_REQUEST_CODE);
  }

}
