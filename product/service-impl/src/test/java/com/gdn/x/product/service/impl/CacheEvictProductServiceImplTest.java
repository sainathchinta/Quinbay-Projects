package com.gdn.x.product.service.impl;

import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gdn.x.product.service.cache.CacheEvictItemServiceImpl;

public class CacheEvictProductServiceImplTest {

  private static final String PRODUCT_SKU = "productSku";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final String STORE_ID = "storeId";
  private static final String ITEM_SKU = "itemSku";


  @InjectMocks
  private CacheEvictItemServiceImpl cacheEvictItemServiceImpl;

  @Test
  public void evictFindItemByItemSkuTest() {
    this.cacheEvictItemServiceImpl.evictFindItemByItemSku(CacheEvictProductServiceImplTest.STORE_ID,
        CacheEvictProductServiceImplTest.ITEM_SKU);
  }

  @Test
  public void evictFindItemByMerchantSkuTest() {
    this.cacheEvictItemServiceImpl.evictFindItemByMerchantSku(
        CacheEvictProductServiceImplTest.STORE_ID, CacheEvictProductServiceImplTest.MERCHANT_SKU);
  }

  @Test
  public void evictFindItemByProductSkuTest() {
    this.cacheEvictItemServiceImpl.evictFindItemByProductSku(
        CacheEvictProductServiceImplTest.STORE_ID, CacheEvictProductServiceImplTest.PRODUCT_SKU);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() {}
}
