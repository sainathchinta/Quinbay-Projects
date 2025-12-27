package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.dao.api.AppealProductRepository;
import com.gdn.x.mta.distributiontask.model.AppealedProduct;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AppealedProductServiceImplTest {

  @InjectMocks
  private AppealProductServiceImpl appealProductService;

  @Mock
  private AppealProductRepository appealProductRepository;

  private static final String PRODUCT_CODE = "productCode";
  private AppealedProduct appealedProduct;

  @BeforeEach
  public void setup() {
    appealedProduct = new AppealedProduct();
    appealedProduct.setProductCode(PRODUCT_CODE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(appealProductRepository);
  }

  @Test
   void upsertAppealProduct() {
    appealedProduct = new AppealedProduct();
    appealProductService.upsertAppealProduct(appealedProduct);
    Mockito.verify(appealProductRepository).save(appealedProduct);
  }

  @Test
   void findAppealProductByProductCode() {
    appealedProduct = new AppealedProduct();
    Mockito.when(appealProductRepository.findByProductCode(PRODUCT_CODE)).thenReturn(
      appealedProduct);
    Assertions.assertEquals(appealProductService.findAppealProductByProductCode(PRODUCT_CODE),
      appealedProduct);
    Mockito.verify(appealProductRepository).findByProductCode(PRODUCT_CODE);
  }
}
