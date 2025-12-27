package com.gdn.mta.product.service;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductBusinessPartnerAttributeRepository;
import com.gdn.mta.product.repository.ProductSuspensionHistoryRepository;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.workflow.WorkflowProcessor;

public class TerminatedSellerSkuCleanupServiceBeanTest {

  @InjectMocks
  private TerminatedSellerSkuCleanupServiceBean terminatedSellerSkuCleanupServiceBean;

  @Mock
  private ProductService productService;

  @Mock
  private ProductAutoApprovalCriteriaService productAutoApprovalCriteriaService;

  @Mock
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Mock
  private ProductWorkflowService productWorkflowService;

  @Mock
  private WorkflowProcessor workflowProcessor;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Mock
  private ProductSuspensionHistoryRepository productSuspensionHistoryRepository;

  @Mock
  private ProductStockAlertService productStockAlertService;

  @Mock
  private ProductBusinessPartnerAttributeRepository productBusinessPartnerAttributeRepository;

  @Mock
  private UpdatedProductHistoryService updatedProductHistoryService;

  private ProductCollection productCollection;

  private static final String ID = "id";
  private static final String PRODUCT_ID = "product_id";
  private static final String PRODUCT_CODE = "product_code";
  private static final String STORE_ID = "10001";

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    productCollection = new ProductCollection();
    productCollection.setId(ID);
    productCollection.setProductId(PRODUCT_ID);
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setPickedForDeletion(true);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productSuspensionHistoryRepository, productService, workflowProcessor,
        productWorkflowService, productImageQcProcessingResponseService,
        productAutoApprovalCriteriaService, updatedProductHistoryService, productBusinessPartnerAttributeRepository,
        productStockAlertService, productItemBusinessPartnerService, productLevel1HistoryService,
        productBusinessPartnerService);
  }

  @Test
  public void terminatedSellerSkuCleanupTest() throws Exception {
    boolean result = terminatedSellerSkuCleanupServiceBean.terminatedSellerSkuCleanup(productCollection);
    Mockito.verify(productAutoApprovalCriteriaService)
        .deleteProductAutoApprovalCriteriaByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcProcessingResponseService)
        .deleteProductImageQcProcessingResponseByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productWorkflowService).deleteProductWorkflowByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(workflowProcessor).deleteProductWfHistoryByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).deleteProductCollectionByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel1HistoryService).deleteProductHistoryByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productSuspensionHistoryRepository).deleteByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productBusinessPartnerAttributeRepository).deleteByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productStockAlertService).deleteProductStockAlertByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(updatedProductHistoryService).deleteUpdateProductHistoryByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemBusinessPartnerService)
        .deleteProductItemBusinessPartnerByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productBusinessPartnerService)
        .deleteProductBusinessPartnerByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Assertions.assertTrue(result);
  }

  @Test
  public void terminatedSellerSkuCleanupExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(productAutoApprovalCriteriaService)
        .deleteProductAutoApprovalCriteriaByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    boolean result = terminatedSellerSkuCleanupServiceBean.terminatedSellerSkuCleanup(productCollection);
    Mockito.verify(productAutoApprovalCriteriaService)
        .deleteProductAutoApprovalCriteriaByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Assertions.assertFalse(result);
  }
}
