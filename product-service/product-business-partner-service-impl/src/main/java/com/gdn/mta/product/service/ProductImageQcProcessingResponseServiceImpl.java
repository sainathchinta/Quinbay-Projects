package com.gdn.mta.product.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.repository.ProductImageQcProcessingResponseRepository;
import com.gdn.partners.pbp.model.vo.CacheKeys;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductImageQcProcessingResponseServiceImpl implements ProductImageQcProcessingResponseService {

  @Autowired
  private ProductImageQcProcessingResponseRepository productImageQcProcessingResponseRepository;

  @Override
  @Cacheable(value = CacheKeys.PRODUCT_IMAGE_PREDICTION_RESPONSE, key = "#productCode", unless = "#result == null")
  public ProductImageQcProcessingResponse findByStoreIdAndProductCode(String storeId, String productCode) {
    return productImageQcProcessingResponseRepository.findByStoreIdAndProductCode(storeId, productCode);
  }

  @Override
  public List<ProductImageQcProcessingResponse> findByStoreIdAndProductCodeIn(String storeId,
      List<String> productCodes) {
    return productImageQcProcessingResponseRepository.findByStoreIdAndProductCodeIn(storeId,productCodes);
  }

  @Override
  public ProductImageQcProcessingResponse findByStoreIdAndProductCodeDb(String storeId, String productCode) {
    return productImageQcProcessingResponseRepository.findByStoreIdAndProductCode(storeId, productCode);
  }

  @Override
  @Transactional(readOnly = false)
  @CacheEvict(value = CacheKeys.PRODUCT_IMAGE_PREDICTION_RESPONSE, key = "#productImageQcProcessingResponse.productCode")
  public void save(ProductImageQcProcessingResponse productImageQcProcessingResponse) {
    productImageQcProcessingResponseRepository.saveAndFlush(productImageQcProcessingResponse);
  }

  @Override
  public void deleteProductImageQcProcessingResponseByStoreIdAndProductCode(String storeId, String productCode){
    productImageQcProcessingResponseRepository.deleteByStoreIdAndProductCode(storeId, productCode);
  }
}
