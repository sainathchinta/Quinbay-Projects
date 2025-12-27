package com.gdn.mta.product.service;

import com.gdn.mta.product.entity.ProductLevel3FailedEntity;
import com.gdn.mta.product.enums.ProductLevel3RetryStatus;
import com.gdn.mta.product.repository.ProductLevel3FailedEntityRepository;
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
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.Arrays;

import static org.mockito.ArgumentMatchers.eq;

public class ProductLevel3RetryServiceImplTest {

  private static final String PRODUCT_SKU = "productSku";
  private static final String STORE_ID = "storeId";
  private static final int RETRY_COUNT = 0;
  private static final int MAX_LIMIT = 10;

  private ProductLevel3FailedEntity productLevel3FailedEntity;

  @Mock
  private ProductLevel3FailedEntityRepository productLevel3FailedEntityRepository;

  @InjectMocks
  private ProductLevel3RetryServiceImpl productLevel3RetryService;

  @Captor
  private ArgumentCaptor<ProductLevel3FailedEntity> productLevel3FailedEntityArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productLevel3FailedEntity =
      ProductLevel3FailedEntity.builder().productSku(PRODUCT_SKU).retryStatus(ProductLevel3RetryStatus.PENDING.name())
        .retryCount(RETRY_COUNT).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productLevel3FailedEntityRepository);
  }

  @Test
  public void upsertProductLevel3FailureLog_emptyCurrentValueTest() {
    productLevel3RetryService.upsertProductLevel3FailureLog(STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.productLevel3FailedEntityRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU);
    Mockito.verify(this.productLevel3FailedEntityRepository).save(productLevel3FailedEntityArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productLevel3FailedEntityArgumentCaptor.getValue().getProductSku());
    Assertions.assertEquals(STORE_ID, productLevel3FailedEntityArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(0, productLevel3FailedEntityArgumentCaptor.getValue().getRetryCount());
  }

  @Test
  public void upsertProductLevel3FailureLog_existingEntryTest() {
    Mockito.when(this.productLevel3FailedEntityRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU)).thenReturn(productLevel3FailedEntity);
    productLevel3RetryService.upsertProductLevel3FailureLog(STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.productLevel3FailedEntityRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU);
    Mockito.verify(this.productLevel3FailedEntityRepository).save(productLevel3FailedEntityArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productLevel3FailedEntityArgumentCaptor.getValue().getProductSku());
    Assertions.assertEquals(ProductLevel3RetryStatus.FAILED.name(), productLevel3FailedEntityArgumentCaptor.getValue().getRetryStatus());
    Assertions.assertEquals(RETRY_COUNT + 1, productLevel3FailedEntityArgumentCaptor.getValue().getRetryCount());
  }

  @Test
  public void upsertProductLevel3FailureLog_existingFailedEntryTest() {
    productLevel3FailedEntity.setRetryStatus(ProductLevel3RetryStatus.FAILED.name());
    Mockito.when(this.productLevel3FailedEntityRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU)).thenReturn(productLevel3FailedEntity);
    productLevel3RetryService.upsertProductLevel3FailureLog(STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.productLevel3FailedEntityRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU);
    Mockito.verify(this.productLevel3FailedEntityRepository).save(productLevel3FailedEntityArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productLevel3FailedEntityArgumentCaptor.getValue().getProductSku());
    Assertions.assertEquals(ProductLevel3RetryStatus.FAILED.name(), productLevel3FailedEntityArgumentCaptor.getValue().getRetryStatus());
    Assertions.assertEquals(RETRY_COUNT + 1, productLevel3FailedEntityArgumentCaptor.getValue().getRetryCount());
  }

  @Test
  public void findProductsForRetryJobTest() {
    Mockito.when(this.productLevel3FailedEntityRepository.findByStoreIdAndRetryCountLessThanEqualAndMarkForDeleteFalse(eq(STORE_ID),
      eq(RETRY_COUNT), Mockito.any(Pageable.class))).thenReturn(new PageImpl<>(Arrays.asList()));
    this.productLevel3RetryService.findProductsForRetryJob(STORE_ID, RETRY_COUNT, MAX_LIMIT);
    Mockito.verify(this.productLevel3FailedEntityRepository).findByStoreIdAndRetryCountLessThanEqualAndMarkForDeleteFalse(STORE_ID,
      RETRY_COUNT, PageRequest.of(0, MAX_LIMIT));
  }

  @Test
  public void updateCompletedOrOmittedStateTest() {
    this.productLevel3RetryService.updateCompletedOrOmittedState(STORE_ID, PRODUCT_SKU,
      ProductLevel3RetryStatus.COMPLETED.name());
    Mockito.verify(this.productLevel3FailedEntityRepository)
      .findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.productLevel3FailedEntityRepository).save(Mockito.any(ProductLevel3FailedEntity.class));
  }

  @Test
  public void updateCompletedOrOmittedState_withExistingObjectTest() {
    Mockito.when(
        this.productLevel3FailedEntityRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
      .thenReturn(ProductLevel3FailedEntity.builder().productSku(PRODUCT_SKU).build());
    this.productLevel3RetryService.updateCompletedOrOmittedState(STORE_ID, PRODUCT_SKU,
      ProductLevel3RetryStatus.COMPLETED.name());
    Mockito.verify(this.productLevel3FailedEntityRepository)
      .findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.productLevel3FailedEntityRepository).save(Mockito.any(ProductLevel3FailedEntity.class));
  }

  @Test
  public void upsertProductLevel3FailureLog_exceptionTest() {
    Mockito.when(this.productLevel3FailedEntityRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU)).thenThrow(RuntimeException.class);
    productLevel3RetryService.upsertProductLevel3FailureLog(STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.productLevel3FailedEntityRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU);
  }

  @Test
  public void findProductsForSendingMailTest() {
    productLevel3FailedEntity.setRetryStatus(ProductLevel3RetryStatus.FAILED.name());
    productLevel3FailedEntity.setRetryCount(3);
    Mockito.when(this.productLevel3FailedEntityRepository
        .findByStoreIdAndRetryCountGreaterThanAndMarkForDeleteFalse(eq(STORE_ID), eq(RETRY_COUNT)))
        .thenReturn(Arrays.asList(productLevel3FailedEntity));
    this.productLevel3RetryService.findProductsForSendingMail(STORE_ID, RETRY_COUNT);
    Mockito.verify(this.productLevel3FailedEntityRepository)
        .findByStoreIdAndRetryCountGreaterThanAndMarkForDeleteFalse(STORE_ID, RETRY_COUNT);
  }

  @Test
  public void updateFailedRetryProductsAfterMail() {
    this.productLevel3RetryService.updateFailedRetryProductsAfterMail(Arrays.asList(productLevel3FailedEntity));
    Mockito.verify(this.productLevel3FailedEntityRepository).saveAll(Arrays.asList(productLevel3FailedEntity));
  }

  @Test
  public void updateRetryProductTest() {
    Mockito.when(this.productLevel3FailedEntityRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(
      STORE_ID, PRODUCT_SKU)).thenReturn(productLevel3FailedEntity);
    this.productLevel3RetryService.updateRetryProduct(STORE_ID, PRODUCT_SKU, 3,
      ProductLevel3RetryStatus.FAILED.name());
    Mockito.verify(this.productLevel3FailedEntityRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(
      STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.productLevel3FailedEntityRepository).save(Mockito.any(ProductLevel3FailedEntity.class));
  }
}