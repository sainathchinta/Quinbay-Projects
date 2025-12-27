package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.Mockito.verify;

import java.util.Collections;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.gdn.x.productcategorybase.repository.ProductItemUomInfoRepository;

@ExtendWith(MockitoExtension.class)
class ProductItemUomInfoServiceBeanTest {

  @Mock
  private ProductItemUomInfoRepository productItemUomInfoRepository;

  @InjectMocks
  private ProductItemUomInfoServiceBean productItemUomInfoServiceBean;

  private final String STORE_ID = "store1";
  private final String PRODUCT_CODE = "productCode";

  @Test
  public void findByStoreIdAndProductCodeAndMarkForDeleteFalseTest() {
    productItemUomInfoServiceBean.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(productItemUomInfoRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void findByStoreIdAndSkuCodeInAndMarkForDeleteFalseTest() {
    productItemUomInfoServiceBean.findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(STORE_ID,
        Collections.singletonList(PRODUCT_CODE));
    verify(productItemUomInfoRepository).findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(STORE_ID,
        Collections.singletonList(PRODUCT_CODE));
  }
} 