package com.gdn.mta.product.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gdn.mta.product.entity.ProductImagePrediction;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductImagePredictionServiceImplWrapper implements ProductImagePredictionServiceWrapper {

  @Autowired
  private ProductImagePredictionService productImagePredictionService;

  public void updateImagePredictionAndCategoryMappingAndCacheEvict(String storeId,
      ProductImagePredictionAndCategoryMappingRequest productImagePrediction) throws Exception {
    ProductImagePrediction prediction =
        productImagePredictionService.updateImagePredictionAndCategoryMapping(storeId, productImagePrediction);
    productImagePredictionService.cacheEvictForUpdateImagePredictionAndCategoryMapping(prediction);
  }

  @Override
  public void update(ProductImagePrediction productImagePrediction){
    productImagePredictionService.update(productImagePrediction);
    productImagePredictionService.evictPredictionTypeCache(productImagePrediction.getStoreId());
  }
}
