package com.gdn.mta.product.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.entity.ProductAutoApprovalCriteria;
import com.gdn.mta.product.repository.ProductAutoApprovalCriteriaRepository;

public class ProductAutoApprovalServiceImplTest {

  @InjectMocks
  private ProductAutoApprovalServiceImpl productAutoApprovalService;

  @Mock
  private ProductAutoApprovalCriteriaRepository productAutoApprovalCriteriaRepository;

  private ProductAutoApprovalCriteria productAutoApprovalCriteria;

  private static final String PRODUCT_CODE = "product_code";
  private static final String STORE_ID = "10001";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productAutoApprovalCriteria = new ProductAutoApprovalCriteria();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productAutoApprovalCriteriaRepository);
  }

  @Test
  public void saveProductAutoApprovalCriteriaTest(){
    productAutoApprovalService.saveProductAutoApprovalCriteria(productAutoApprovalCriteria);
    verify(productAutoApprovalCriteriaRepository).save(any(ProductAutoApprovalCriteria.class));
  }

  @Test
  public void deleteProductAutoApprovalCriteriaByStoreIdAndProductCodeTest() {
    productAutoApprovalService.deleteProductAutoApprovalCriteriaByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(productAutoApprovalCriteriaRepository).deleteByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

}
