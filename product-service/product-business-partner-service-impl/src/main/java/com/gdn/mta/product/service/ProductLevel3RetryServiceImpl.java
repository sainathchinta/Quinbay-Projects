package com.gdn.mta.product.service;

import com.gdn.mta.product.entity.ProductLevel3FailedEntity;
import com.gdn.mta.product.enums.ProductLevel3RetryStatus;
import com.gdn.mta.product.repository.ProductLevel3FailedEntityRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Service
@Slf4j
public class ProductLevel3RetryServiceImpl implements ProductLevel3RetryService {

  @Autowired
  private ProductLevel3FailedEntityRepository productLevel3FailedEntityRepository;

  @Async
  @Override
  public void upsertProductLevel3FailureLog(String storeId, String productSku) {
    try {
      ProductLevel3FailedEntity productLevel3FailedEntity =
        this.productLevel3FailedEntityRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
      if (Objects.isNull(productLevel3FailedEntity)) {
        productLevel3FailedEntity = ProductLevel3FailedEntity.builder().productSku(productSku).retryCount(0)
          .retryStatus(ProductLevel3RetryStatus.PENDING.name()).build();
        productLevel3FailedEntity.setStoreId(storeId);
      } else {
        if (ProductLevel3RetryStatus.PENDING.name().equals(productLevel3FailedEntity.getRetryStatus())) {
          productLevel3FailedEntity.setRetryStatus(ProductLevel3RetryStatus.FAILED.name());
          productLevel3FailedEntity.setRetryCount(1);
        } else {
          productLevel3FailedEntity.setRetryCount(productLevel3FailedEntity.getRetryCount() + 1);
        }
      }
      productLevel3FailedEntityRepository.save(productLevel3FailedEntity);
    } catch (Exception e) {
      log.error("Error on updating status of product sku : {}", productSku, e);
    }
  }

  @Override
  public List<ProductLevel3FailedEntity> findProductsForRetryJob(String storeId, int maxRetryCount, int limit) {
    Page<ProductLevel3FailedEntity> retryProducts =
      this.productLevel3FailedEntityRepository.findByStoreIdAndRetryCountLessThanEqualAndMarkForDeleteFalse(storeId, maxRetryCount,
        PageRequest.of(0, limit));
    return retryProducts.getContent();
  }

  @Override
  public List<ProductLevel3FailedEntity> findProductsForSendingMail(String storeId, int maxRetryCount) {
    return this.productLevel3FailedEntityRepository
        .findByStoreIdAndRetryCountGreaterThanAndMarkForDeleteFalse(storeId, maxRetryCount);
  }

  @Override
  @Transactional(readOnly = false)
  public void updateFailedRetryProductsAfterMail(List<ProductLevel3FailedEntity> productLevel3FailedEntityList) {
    this.productLevel3FailedEntityRepository.saveAll(productLevel3FailedEntityList);
  }

  @Override
  @Transactional
  public void updateRetryProduct(String storeId, String productSku, int retryCount, String state) {
    ProductLevel3FailedEntity productLevel3FailedEntity =
      this.productLevel3FailedEntityRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(
        storeId, productSku);
    productLevel3FailedEntity.setRetryCount(retryCount);
    productLevel3FailedEntity.setRetryStatus(state);
    this.productLevel3FailedEntityRepository.save(productLevel3FailedEntity);
  }

  @Override
  public void updateCompletedOrOmittedState(String storeId, String gdnSku, String status) {
    ProductLevel3FailedEntity productLevel3FailedEntity =
      this.productLevel3FailedEntityRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, gdnSku);
    if (Objects.isNull(productLevel3FailedEntity)) {
      productLevel3FailedEntity = ProductLevel3FailedEntity.builder().productSku(gdnSku).build();
      productLevel3FailedEntity.setStoreId(storeId);
    }
    productLevel3FailedEntity.setRetryStatus(status);
    productLevel3FailedEntity.setMarkForDelete(true);
    productLevel3FailedEntityRepository.save(productLevel3FailedEntity);
  }
}
