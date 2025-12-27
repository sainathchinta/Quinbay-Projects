package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.dao.api.ProductActionRetryRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductAttributeRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductAutoApprovalRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductImageQcFeedbackRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductImageRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemAttributeRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemImageRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductReviewerRepository;
import com.gdn.x.mta.distributiontask.dao.api.TaskHistoryRepository;
import com.gdn.x.mta.distributiontask.model.Product;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.Date;

@ExtendWith(MockitoExtension.class)
class ProductsPermanentDeleteServiceImplTest {

  private static final String PRODUCT_CODE = "TEST_PRODUCT";
  private static final String STORE_ID = "10001";
  private static final String ITEM_CODE = "ITEM_CODE";
  private static final String ID = "ID";

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductItemRepository productItemRepository;

  @Mock
  private ProductItemImageRepository productItemImageRepository;

  @Mock
  private ProductItemAttributeRepository productItemAttributeRepository;

  @Mock
  private ProductReviewerRepository productReviewerRepository;

  @Mock
  private ProductImageQcFeedbackRepository productImageQcFeedbackRepository;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private ProductAutoApprovalRepository productAutoApprovalRepository;

  @Mock
  private ProductActionRetryRepository productActionRetryRepository;

  @Mock
  private TaskHistoryRepository taskHistoryRepository;

  @Mock
  private ProductImageRepository productImageRepository;

  @Mock
  private ProductAttributeRepository productAttributeRepository;

  @InjectMocks
  private ProductsPermanentDeleteServiceImpl productService;

  private Product product;

  @BeforeEach
  public void setUp() {
    product = new Product();
    product.setId(ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productAttributeRepository);
    Mockito.verifyNoMoreInteractions(productImageRepository);
    Mockito.verifyNoMoreInteractions(taskHistoryRepository);
    Mockito.verifyNoMoreInteractions(productActionRetryRepository);
    Mockito.verifyNoMoreInteractions(productAutoApprovalRepository);
    Mockito.verifyNoMoreInteractions(productDistributionTaskRepository);
    Mockito.verifyNoMoreInteractions(productImageQcFeedbackRepository);
    Mockito.verifyNoMoreInteractions(productReviewerRepository);
    Mockito.verifyNoMoreInteractions(productRepository);
    Mockito.verifyNoMoreInteractions(productItemAttributeRepository);
    Mockito.verifyNoMoreInteractions(productItemImageRepository);
    Mockito.verifyNoMoreInteractions(productItemRepository);
  }

  @Test
  void testProcessProductsPermanentDelete_NotPickedForDeletion() {
    product.setPickedForDeletion(false);
    product.setUpdatedDate(new Date());
    Mockito.when(productItemRepository.findIdByProduct(ID))
      .thenReturn(Collections.singletonList(ITEM_CODE));
    productService.deleteProducts(PRODUCT_CODE, ID, STORE_ID);
    Mockito.verify(productItemRepository).findIdByProduct(ID);
    Mockito.verify(productAttributeRepository).deleteByProductIds(Collections.singletonList(ID));
    Mockito.verify(productImageRepository).deleteByProductIds(Collections.singletonList(ID));
    Mockito.verify(taskHistoryRepository)
      .deleteByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productActionRetryRepository).deleteByStoreIdAndProductCode(STORE_ID,
      PRODUCT_CODE);
    Mockito.verify(productAutoApprovalRepository).deleteByProductCode(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskRepository)
      .deleteByProductIds(Collections.singletonList(ID));
    Mockito.verify(productImageQcFeedbackRepository).deleteByStoreIdAndProductCode(STORE_ID,
      PRODUCT_CODE);
    Mockito.verify(productReviewerRepository)
      .deleteByProductCodeIn(Collections.singletonList(PRODUCT_CODE));
    Mockito.verify(productRepository).deleteById(Collections.singletonList(ID));
    Mockito.verify(productItemAttributeRepository)
      .deleteByProductItemIds(Collections.singletonList(ITEM_CODE));
    Mockito.verify(productItemImageRepository)
      .deleteByProductItemIds(Collections.singletonList(ITEM_CODE));
    Mockito.verify(productItemRepository).deleteByProductIds(Collections.singletonList(ID));
  }

  @Test
  void testProcessProductsPermanentDelete_PickedForDeletion_UpdatedTime() {
    product.setPickedForDeletion(true);
    Date updatedDate = new Date(System.currentTimeMillis() - (31 * 60 * 1000));
    product.setUpdatedDate(updatedDate);
    Mockito.when(productItemRepository.findIdByProduct(ID))
      .thenReturn(null);
    productService.deleteProducts(PRODUCT_CODE, ID, STORE_ID);
    Mockito.verify(productItemRepository).findIdByProduct(ID);
    Mockito.verify(productAttributeRepository).deleteByProductIds(Collections.singletonList(ID));
    Mockito.verify(productImageRepository).deleteByProductIds(Collections.singletonList(ID));
    Mockito.verify(taskHistoryRepository)
      .deleteByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productActionRetryRepository).deleteByStoreIdAndProductCode(STORE_ID,
      PRODUCT_CODE);
    Mockito.verify(productAutoApprovalRepository).deleteByProductCode(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskRepository)
      .deleteByProductIds(Collections.singletonList(ID));
    Mockito.verify(productImageQcFeedbackRepository).deleteByStoreIdAndProductCode(STORE_ID,
      PRODUCT_CODE);
    Mockito.verify(productReviewerRepository)
      .deleteByProductCodeIn(Collections.singletonList(PRODUCT_CODE));
    Mockito.verify(productRepository).deleteById(Collections.singletonList(ID));
    Mockito.verify(productItemRepository).deleteByProductIds(Collections.singletonList(ID));
  }
}
