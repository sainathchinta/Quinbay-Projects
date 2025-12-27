package com.gdn.mta.product.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gdn.mta.product.entity.ProductImagePrediction;

public class ProductImagePredictionServiceImplWrapperTest {

  private static final String STORE_ID = "10001";

  @InjectMocks
  private ProductImagePredictionServiceImplWrapper productImagePredictionServiceImplWrapper;

  @Mock
  private ProductImagePredictionService productImagePredictionService;

  private ProductImagePrediction productImagePrediction;

  private static final String PREDICTION_TYPE = "PREDICTION_TYPE";


  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void updateImagePredictionAndCategoryMappingAndCacheEvictTest() throws Exception {
    ProductImagePredictionAndCategoryMappingRequest request = new ProductImagePredictionAndCategoryMappingRequest();
    Mockito.when(
            productImagePredictionService.updateImagePredictionAndCategoryMapping(Mockito.anyString(), Mockito.any()))
        .thenReturn(new ProductImagePrediction());
    this.productImagePredictionServiceImplWrapper.updateImagePredictionAndCategoryMappingAndCacheEvict(STORE_ID,
        request);
    Mockito.verify(productImagePredictionService)
        .updateImagePredictionAndCategoryMapping(Mockito.anyString(), Mockito.any());
    Mockito.verify(productImagePredictionService)
        .cacheEvictForUpdateImagePredictionAndCategoryMapping(new ProductImagePrediction());
  }

  @Test
  public void updateProductImagePrediction() throws Exception {
    productImagePrediction =
        ProductImagePrediction.builder().predictionType(PREDICTION_TYPE).build();
    productImagePrediction.setStoreId(STORE_ID);
    this.productImagePredictionServiceImplWrapper.update(productImagePrediction);
    Mockito.verify(productImagePredictionService).update(productImagePrediction);
    Mockito.verify(productImagePredictionService).evictPredictionTypeCache(STORE_ID);
  }
}
