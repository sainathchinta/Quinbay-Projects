package com.gdn.mta.product.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.entity.ProductImageQcBacklog;
import com.gdn.mta.product.repository.ProductImageQcBacklogRepository;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductImageQcBacklogServiceImpl implements ProductImageQcBacklogService {

  @Autowired
  private ProductImageQcBacklogRepository productImageQcBacklogRepository;

  @Override
  public List<ProductImageQcBacklog> findProductImageQcBacklogByStoreIdAndStatus(String storeId, String status,
      String orderProductsBy, String orderProductsIn, int batchSize) {
    return productImageQcBacklogRepository.findByStoreIdAndStatus(storeId, status, orderProductsBy, batchSize);
  }

  @Override
  public ProductImageQcBacklog findByStoreIdAndProductCodeAndStatus(String storeId, String productCode, String status) {
    return productImageQcBacklogRepository.findByStoreIdAndProductCodeAndStatus(storeId, productCode, status);
  }

  @Override
  @Transactional(readOnly = false)
  public ProductImageQcBacklog saveProductImageQcBacklog(ProductImageQcBacklog productImageQcBacklog) {
    return productImageQcBacklogRepository.save(productImageQcBacklog);
  }
}
