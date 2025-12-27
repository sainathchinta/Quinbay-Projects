package com.gdn.mta.product.service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.partners.pbp.entity.workflow.product.ProductWf;
import com.gdn.partners.pbp.repository.workflow.ProductWfRepository;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class ProductWorkflowServiceBeanTest {

  private static final String DEFAULT_PRODUCT_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final Date DEFAULT_CURRENT_TIMESTAMP = Calendar.getInstance().getTime();
  private static final String DEFAULT_STORE_ID = "10001";
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);
  private static final String DEFAULT_PRODUCT_NAME = "Product Testing";
  private static final String DEFAULT_WORKFLOW_STATE = "IN_VENDOR";
  private static final String PRODUCT_CODE = "product_code";
  private static final String STORE_ID = "10001";

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductWorkflowRepository productWorkflowRepository;

  @Mock
  private ProductHistoryRepository productHistoryRepository;

  @InjectMocks
  private ProductWorkflowServiceBean productWorkflowServiceBean;

  @Mock
  private ProductWfRepository productWfRepository;

  @Captor
  private ArgumentCaptor<ProductWf> productWfArgumentCaptor;

  private List<ProductWorkflow> generateProductWorkflows() {
    List<ProductWorkflow> productWorkflows = new ArrayList<ProductWorkflow>();
    productWorkflows.add(new ProductWorkflow(ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_ID,
        ProductWorkflowLookup.STATE_REVIEW_CONTENT,
        ProductWorkflowLookup.STATE_REVIEW_CONTENT_DESCRIPTION, GdnBaseLookup.DEFAULT_USERNAME,
        ProductWorkflowServiceBeanTest.DEFAULT_CURRENT_TIMESTAMP,
        ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID));
    productWorkflows.add(new ProductWorkflow(ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_ID,
        ProductWorkflowLookup.STATE_REVIEW_IMAGE,
        ProductWorkflowLookup.STATE_REVIEW_IMAGE_DESCRIPTION, GdnBaseLookup.DEFAULT_USERNAME,
        ProductWorkflowServiceBeanTest.DEFAULT_CURRENT_TIMESTAMP,
        ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID));
    return productWorkflows;
  }
  
  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setName(ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_NAME);
    Mockito.when(this.productRepository.findProductDetailByProductCode(Mockito.any()))
        .thenReturn(productData);
    Mockito
        .when(this.productWorkflowRepository.saveAndFlush((ProductWorkflow) Mockito.any())).thenReturn(null);
    Mockito.when(this.productHistoryRepository.saveAndFlush((ProductHistory) Mockito.any())).thenReturn(null);
    Mockito.doNothing().when(this.productWorkflowRepository).delete( Mockito.any());
    Mockito.doNothing().when(this.productWorkflowRepository).deleteAll(Mockito.any());
    Mockito.when(this.productHistoryRepository.saveAll( Mockito.any())).thenReturn(null);
    Mockito.when(this.productWorkflowRepository.saveAll(Mockito.any())).thenReturn(null);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productRepository);
    Mockito.verifyNoMoreInteractions(this.productWorkflowRepository);
    Mockito.verifyNoMoreInteractions(this.productHistoryRepository);
    Mockito.verifyNoMoreInteractions(this.productWfRepository);
  }

  @Test
  public void processImageTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    this.productWorkflowServiceBean.processImage(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
        ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.productWorkflowRepository).saveAndFlush(
        (ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository)
        .saveAndFlush((ProductHistory) Mockito.any());
  }

  @Test
  public void processImageWithEmptyProductWorkflowsTest() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<ProductWorkflow>();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.processImage(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductHistory) Mockito.any());
  }

  @Test
  public void processImageWithoutReviewImageProductWorkflowTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    productWorkflows.remove(1);
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.processImage(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductHistory) Mockito.any());
  }

  @Test
  public void processImageWithProcessImageProductWorkflowTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    productWorkflows.add(new ProductWorkflow(ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_ID,
        ProductWorkflowLookup.STATE_PROCESS_IMAGE,
        ProductWorkflowLookup.STATE_PROCESS_IMAGE_DESCRIPTION, GdnBaseLookup.DEFAULT_USERNAME,
        ProductWorkflowServiceBeanTest.DEFAULT_CURRENT_TIMESTAMP,
        ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID));
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.processImage(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductHistory) Mockito.any());
  }

  @Test
  public void rejectProcessImageTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    productWorkflows.add(new ProductWorkflow(ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_ID,
        ProductWorkflowLookup.STATE_PROCESS_IMAGE,
        ProductWorkflowLookup.STATE_PROCESS_IMAGE_DESCRIPTION, GdnBaseLookup.DEFAULT_USERNAME,
        ProductWorkflowServiceBeanTest.DEFAULT_CURRENT_TIMESTAMP,
        ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID));
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    this.productWorkflowServiceBean.rejectProcessImage(
        ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
        ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.productWorkflowRepository).delete((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository)
        .saveAndFlush((ProductHistory) Mockito.any());
  }

  @Test
  public void rejectProcessImageWithEmptyProductWorkflowsTest() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<ProductWorkflow>();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.rejectProcessImage(
            ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .delete((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductHistory) Mockito.any());
  }

  @Test
  public void rejectProcessImageWithoutReviewImageProductWorkflowTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    productWorkflows.remove(1);
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.rejectProcessImage(
            ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .delete((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductHistory) Mockito.any());
  }

  @Test
  public void rejectProcessImageWithoutProcessImageProductWorkflowTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.rejectProcessImage(
            ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .delete((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductHistory) Mockito.any());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveImageTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    productWorkflows.add(new ProductWorkflow(ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_ID,
        ProductWorkflowLookup.STATE_PROCESS_IMAGE,
        ProductWorkflowLookup.STATE_PROCESS_IMAGE_DESCRIPTION, GdnBaseLookup.DEFAULT_USERNAME,
        ProductWorkflowServiceBeanTest.DEFAULT_CURRENT_TIMESTAMP,
        ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID));
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    this.productWorkflowServiceBean.approveImage(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
        ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository).deleteAll(
        (List<ProductWorkflow>) Mockito.any());
    Mockito.verify(this.productHistoryRepository).saveAll((List<ProductHistory>) Mockito.any());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveImageAndIsActiveTrueTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    productWorkflows.remove(0);
    productWorkflows.add(new ProductWorkflow(ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_ID,
        ProductWorkflowLookup.STATE_PROCESS_IMAGE,
        ProductWorkflowLookup.STATE_PROCESS_IMAGE_DESCRIPTION, GdnBaseLookup.DEFAULT_USERNAME,
        ProductWorkflowServiceBeanTest.DEFAULT_CURRENT_TIMESTAMP,
        ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID));
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    this.productWorkflowServiceBean.approveImage(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
        ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.productWorkflowRepository).saveAndFlush(
        (ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository).deleteAll(
        (List<ProductWorkflow>) Mockito.any());
    Mockito.verify(this.productHistoryRepository).saveAll((List<ProductHistory>) Mockito.any());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveImageWithEmptyProductWorkflowsTest() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<ProductWorkflow>();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.approveImage(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .deleteAll((List<ProductWorkflow>) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAll((List<ProductHistory>) Mockito.any());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveImageWithoutReviewImageProductWorkflowTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    productWorkflows.remove(1);
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.approveImage(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .deleteAll((List<ProductWorkflow>) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAll((List<ProductHistory>) Mockito.any());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveImageWithoutProcessImageProductWorkflowTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.approveImage(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .deleteAll((List<ProductWorkflow>) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAll((List<ProductHistory>) Mockito.any());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveContentTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    this.productWorkflowServiceBean.approveContent(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
        ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository).delete((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository).saveAll((List<ProductHistory>) Mockito.any());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void approveContentAndIsActiveTrueTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    productWorkflows.remove(1);
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    this.productWorkflowServiceBean.approveContent(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
        ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.productWorkflowRepository).saveAndFlush(
        (ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository).delete((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository).saveAll((List<ProductHistory>) Mockito.any());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void approveContentWithEmptyProductWorkflowsTest() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<ProductWorkflow>();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.approveContent(
            ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .delete((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAll((List<ProductHistory>) Mockito.any());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void approveContentWithoutReviewContentProductWorkflowTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    productWorkflows.remove(0);
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.approveContent(
            ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE, false);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .delete((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAll((List<ProductHistory>) Mockito.any());
  }
  
  @Test
  public void createTest() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<ProductWorkflow>();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    this.productWorkflowServiceBean.create(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
        ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.productWorkflowRepository).saveAndFlush(
        (ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository)
        .saveAndFlush((ProductHistory) Mockito.any());
  }
  
  @Test
  public void createWithNotEmptyProductWorkflowsTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.create(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAndFlush((ProductHistory) Mockito.any());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void submitTest() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<ProductWorkflow>();
    productWorkflows.add(new ProductWorkflow(ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_ID,
        ProductWorkflowLookup.STATE_DRAFT, ProductWorkflowLookup.STATE_DRAFT_DESCRIPTION,
        GdnBaseLookup.DEFAULT_USERNAME, ProductWorkflowServiceBeanTest.DEFAULT_CURRENT_TIMESTAMP,
        ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID));
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    this.productWorkflowServiceBean.submit(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
        ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.productWorkflowRepository).delete((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .saveAll((List<ProductWorkflow>) Mockito.any());
    Mockito.verify(this.productHistoryRepository).saveAll((List<ProductHistory>) Mockito.any());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void submitWithEmptyProductWorkflowsTest() throws Exception {
    List<ProductWorkflow> productWorkflows = new ArrayList<ProductWorkflow>();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.submit(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .delete((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAll((List<ProductWorkflow>) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAll((List<ProductHistory>) Mockito.any());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void submitWithoutDraftProductWorkflowTest() throws Exception {
    List<ProductWorkflow> productWorkflows = generateProductWorkflows();
    Mockito.when(
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productWorkflows);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productWorkflowServiceBean.submit(ProductWorkflowServiceBeanTest.DEFAULT_STORE_ID,
            ProductWorkflowServiceBeanTest.DEFAULT_PRODUCT_CODE);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productRepository).findProductDetailByProductCode(Mockito.any());
    Mockito.verify(this.productWorkflowRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .delete((ProductWorkflow) Mockito.any());
    Mockito.verify(this.productWorkflowRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAll((List<ProductWorkflow>) Mockito.any());
    Mockito.verify(this.productHistoryRepository, ProductWorkflowServiceBeanTest.NEVER_CALLED)
        .saveAll((List<ProductHistory>) Mockito.any());
  }

  @Test
  public void getProductWfByProductCodesTest() {
    List<String> productCodes = new ArrayList<>();
    productCodes.add("MTA-0000001");
    ProductWf productWorkflow = new ProductWf();
    productWorkflow.setProductCode("MTA-0000001");
    productWorkflow.setState(DEFAULT_WORKFLOW_STATE);
    List<ProductWf> productWorkflows = new ArrayList<ProductWf>();
    productWorkflows.add(productWorkflow);
    Map<String, List<ProductWf>> productWfByProductCodes = new HashMap<>();
    productWfByProductCodes.put(productCodes.get(0),productWorkflows);
    Mockito.when(this.productWfRepository
        .findAllByStoreIdAndProductCodeInAndMarkForDeleteFalse(Mockito.any(),
            Mockito.eq(Arrays.asList("MTA-0000001")))).thenReturn(productWorkflows);
    Map<String, List<ProductWf>> productWfByProductCodesList =
        productWorkflowServiceBean.getProductWfByProductCodes(productCodes);
    Mockito.verify(this.productWfRepository)
        .findAllByStoreIdAndProductCodeInAndMarkForDeleteFalse(Mockito.any(),
            Mockito.eq(Arrays.asList("MTA-0000001")));
    Assertions.assertEquals(productWfByProductCodes,productWfByProductCodesList);
  }

  @Test
  public void getRejectedNotesByProductIdsTest() {
    Object[] objects = {DEFAULT_PRODUCT_ID, DEFAULT_PRODUCT_NAME};
    List<Object[]> list = new ArrayList<>();
    list.add(objects);
    Mockito.when(productWorkflowRepository.findByStoreIdAndProductIdIn(Arrays.asList(DEFAULT_PRODUCT_ID)))
        .thenReturn(list);
    this.productWorkflowServiceBean.getRejectedNotesByProductIds(Arrays.asList(DEFAULT_PRODUCT_ID));
    Mockito.verify(productWorkflowRepository).findByStoreIdAndProductIdIn(Arrays.asList(DEFAULT_PRODUCT_ID));
  }

  @Test
  public void getRejectedNotesByProductIdsEmptyTest() {
    List<Object[]> list = new ArrayList<>();
    Mockito.when(productWorkflowRepository.findByStoreIdAndProductIdIn(Arrays.asList(DEFAULT_PRODUCT_ID)))
        .thenReturn(list);
    this.productWorkflowServiceBean.getRejectedNotesByProductIds(Arrays.asList(DEFAULT_PRODUCT_ID));
    Mockito.verify(productWorkflowRepository).findByStoreIdAndProductIdIn(Arrays.asList(DEFAULT_PRODUCT_ID));
  }

  @Test
  public void deleteProductWorkflowByStoreIdAndProductCodeTest() {
    productWorkflowServiceBean.deleteProductWorkflowByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productWfRepository).deleteByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }
}
