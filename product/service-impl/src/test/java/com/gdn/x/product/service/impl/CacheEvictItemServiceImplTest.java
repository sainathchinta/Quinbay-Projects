package com.gdn.x.product.service.impl;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;

import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.MasterDataAttributeService;
import com.gdn.x.product.service.cache.CacheEvictItemServiceImpl;
import com.gdn.x.product.service.cache.CacheItemHelperServiceImpl;

public class CacheEvictItemServiceImplTest {

  private static final String ITEM_SKU = "itemSku";
  private static final String PRODUCT_SKU = "productSku";
  private static final String STORE_ID = "storeId";
  private static final String PRISTINE_ID = "pristineId";
  private static final String ITEM_CODE = "itemCode";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";

  @InjectMocks
  private CacheItemHelperServiceImpl cacheEvictProductServiceImpl;

  @InjectMocks
  private CacheEvictItemServiceImpl cacheEvictItemServiceImpl;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private MasterDataAttributeService masterDataAttributeService;

  @Mock
  private RedisTemplate<String, Item> redisTemplate;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private LettuceConnectionFactory lettuceUniqueIdCheckConnectionFactory;

  @Mock
  private ApplicationContext applicationContext;

  @Test
  public void findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalseTest() {
    Mockito.when(applicationContext.getBean(CacheItemHelperService.class)).thenReturn(cacheItemHelperService);
    this.cacheEvictProductServiceImpl.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(
        CacheEvictItemServiceImplTest.STORE_ID, CacheEvictItemServiceImplTest.ITEM_SKU);
    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndItemSku(CacheEvictItemServiceImplTest.STORE_ID,
            CacheEvictItemServiceImplTest.ITEM_SKU);
    verify(this.applicationContext).getBean(CacheItemHelperService.class);
  }

  @Test
  public void findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse() {
    this.cacheEvictProductServiceImpl.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(
        CacheEvictItemServiceImplTest.STORE_ID, CacheEvictItemServiceImplTest.PRODUCT_SKU);
    verify(this.itemRepository).findItemsByStoreIdAndProductSkuAndMarkForDelete(
        CacheEvictItemServiceImplTest.STORE_ID, CacheEvictItemServiceImplTest.PRODUCT_SKU, Boolean.FALSE, false);
  }

  @Test
  public void findCacheableByStoreIdAndProductSkuTest() {
    this.cacheEvictProductServiceImpl.findCacheableByStoreIdAndProductSku(
        CacheEvictItemServiceImplTest.STORE_ID, CacheEvictItemServiceImplTest.PRODUCT_SKU);
    verify(this.itemRepository).findItemsByStoreIdAndProductSku(
        CacheEvictItemServiceImplTest.STORE_ID, CacheEvictItemServiceImplTest.PRODUCT_SKU);
  }

  @Test
  public void evictFindItemByPristineIdTest(){
    this.cacheEvictItemServiceImpl
        .evictFindItemSkusByPristineId(CacheEvictItemServiceImplTest.STORE_ID, PRISTINE_ID);
    Mockito.verify(redisTemplate).delete(Mockito.anyString());
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID, PRISTINE_ID);

  }

  @Test
  public void evictFindItemByPristineId_whenKeyNotExistTest(){
    this.cacheEvictItemServiceImpl
        .evictFindItemSkusByPristineId(CacheEvictItemServiceImplTest.STORE_ID, PRISTINE_ID);
    Mockito.verify(redisTemplate).delete(Mockito.anyString());
    verify(cacheItemHelperService).findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(CacheEvictItemServiceImplTest.STORE_ID, PRISTINE_ID);
  }

  @Test
  public void evictFindItemCodesByPristineTest(){
    this.cacheEvictItemServiceImpl
        .evictFindItemCodesByPristine(CacheEvictItemServiceImplTest.STORE_ID, PRISTINE_ID);
  }

  @Test
  public void evictFindL5ByItemSku() {
    this.cacheEvictItemServiceImpl.evictFindL5ByItemSku(CacheEvictItemServiceImplTest.STORE_ID, ITEM_SKU);
  }

  @Test
  public void evictFindItemPickupPointByItemSkuAndPickupPointCode() {
    this.cacheEvictItemServiceImpl
      .evictFindItemPickupPointByItemSkuAndPickupPointCode(CacheEvictItemServiceImplTest.STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE);
  }

  @Test
  public void evictFindAllActivePromoBundlingsByPristineId() {
    this.cacheEvictItemServiceImpl
        .evictFindAllActivePromoBundlingsByPristineId(CacheEvictItemServiceImplTest.STORE_ID,
            PRISTINE_ID, PICKUP_POINT_CODE);
  }

  @Test
  public void evictFindAllActivePromoBundlingsByItemCode() {
    this.cacheEvictItemServiceImpl
        .evictFindAllActivePromoBundlingsByItemCode(CacheEvictItemServiceImplTest.STORE_ID,
            ITEM_CODE, PICKUP_POINT_CODE);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.itemRepository);
    verifyNoMoreInteractions(this.redisTemplate);
    verifyNoMoreInteractions(this.cacheItemHelperService);
    verifyNoMoreInteractions(this.cacheEvictHelperService);
    verifyNoMoreInteractions(this.lettuceUniqueIdCheckConnectionFactory);
  }
}
