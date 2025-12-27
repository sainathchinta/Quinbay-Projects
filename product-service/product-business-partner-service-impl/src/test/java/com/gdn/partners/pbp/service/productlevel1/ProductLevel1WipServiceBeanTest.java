package com.gdn.partners.pbp.service.productlevel1;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.entity.ProductReviewStatus;

/**
 * Created by vishal on 18/07/17.
 */
public class ProductLevel1WipServiceBeanTest {

  @InjectMocks
  ProductLevel1WipServiceBean productLevel1WipServiceBean;

  @Mock
  private ProductLevel1CollectionService productLevel1CollectionService;

  private static final String DEFAULT_PRODUCT_CODE = "mta-134";


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productLevel1CollectionService);
  }

  @Test
  public void testUpdateRejectedProduct_success() throws Exception {
    productLevel1WipServiceBean.updateRejectedProduct(DEFAULT_PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .updateRejectedProduct(Mockito.eq(DEFAULT_PRODUCT_CODE));
  }

  @Test
  public void testUpdateRejectedProduct_fail() throws Exception {
    Mockito.doThrow(new Exception()).when(productLevel1CollectionService)
        .updateRejectedProduct(DEFAULT_PRODUCT_CODE);
    try {
      productLevel1WipServiceBean.updateRejectedProduct(DEFAULT_PRODUCT_CODE);
    }catch (Exception e){
    Mockito.verify(productLevel1CollectionService)
        .updateRejectedProduct(Mockito.eq(DEFAULT_PRODUCT_CODE));
    }
  }

}
