package com.gdn.x.product.service.cache;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.util.CollectionUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;

public class CacheItemHelperServiceImplTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String STORE_ID = "storeId";
  private static final String PRISTINE_ID = "pristineId";
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_CODE = "itemCode";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  @InjectMocks
  private CacheEvictProductServiceImpl cacheEvictProductServiceImpl;

  @InjectMocks
  private CacheItemHelperServiceImpl cacheItemHelperServiceImpl;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private PristineItemRepository pristineItemRepository;

  @Mock
  private RedisTemplate<String, Item> redisTemplate;

  @Mock
  private ValueOperations valueOperations;

  @Mock
  private ApplicationContext applicationContext;

  private PristineDataItem pristineDataItem;
  private Item item;
  private Set<String> activePromoBundlings;
  private Set<String> itemCodes;

  @Test
  public void evictFindProductByProductCodeTest() {
    this.cacheEvictProductServiceImpl.evictFindProductByProductCode(
        CacheItemHelperServiceImplTest.STORE_ID, CacheItemHelperServiceImplTest.PRODUCT_CODE);
  }

  @Test
  public void evictFindProductByProductSkuTest() {
    this.cacheEvictProductServiceImpl.evictFindProductByProductSku(
        CacheItemHelperServiceImplTest.STORE_ID, CacheItemHelperServiceImplTest.PRODUCT_SKU);
  }

  @Test
  public void findCacheableItemsByStoreIdAndPristineIdAndMarkForDeleteFalse_WhenStoreIdNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.cacheItemHelperServiceImpl
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(null, PRISTINE_ID));
  }

  @Test
  public void findCacheableItemsByStoreIdAndPristineId_WhenPristineIdsNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.cacheItemHelperServiceImpl
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID, null));
  }

  @Test
  public void findCacheableItemsByStoreIdAndPristineId_WhenPristineIsNullTest() {
    when(pristineItemRepository.findByPristineId(PRISTINE_ID)).thenReturn(null);
    Set<String> itemSkus = this.cacheItemHelperServiceImpl
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID, PRISTINE_ID);
    verify(pristineItemRepository).findByPristineId(PRISTINE_ID);
    Assertions.assertTrue(CollectionUtils.isEmpty(itemSkus));
  }

  @Test
  public void findCacheableItemsByStoreIdAndPristineIdAndMarkForDeleteFalseTest() throws Exception {

    List<Item> listOfItems = new ArrayList<>();
    PristineDataItem pristineItem = new PristineDataItem();
    List<PristineDataItem> pristineItemList = new ArrayList<>();
    pristineItemList.add(pristineItem);
    when(pristineItemRepository.findByPristineId(PRISTINE_ID)).thenReturn(pristineItem);
    when(this.itemRepository
        .findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
            eq(pristineItem))).thenReturn(listOfItems);
    Set<String> itemSkus =  this.cacheItemHelperServiceImpl
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID, PRISTINE_ID);
    verify(pristineItemRepository).findByPristineId(PRISTINE_ID);
    verify(this.itemRepository).findItemSkusByPristine(eq(STORE_ID), eq(pristineItem));
    Assertions.assertEquals(itemSkus.size(), listOfItems.size());

  }

  @Test
  public void findCacheableActivePromoBundlingsByStoreIdAndPristine_success() {
    when(itemRepository.findItemSkusByPristine(STORE_ID, this.pristineDataItem))
        .thenReturn(this.activePromoBundlings);
    when(itemPickupPointService.getActivePromoBundlingByItemSkus(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(this.activePromoBundlings);
    Set<String> activePromoBundlings = this.cacheItemHelperServiceImpl
        .findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, this.pristineDataItem);

    verify(itemRepository).findItemSkusByPristine(STORE_ID, this.pristineDataItem);
    verify(itemPickupPointService).getActivePromoBundlingByItemSkus(eq(STORE_ID), Mockito.anyList());
    Assertions.assertNotNull(activePromoBundlings);
    Assertions.assertEquals(this.activePromoBundlings, activePromoBundlings);
  }

  @Test
  public void findCacheableActivePromoBundlingsByStoreIdAndPristineItemSkusEmptysuccess() {
    when(itemRepository.findItemSkusByPristine(eq(STORE_ID), Mockito.any()))
        .thenReturn(new HashSet<>());
    when(itemPickupPointService.getActivePromoBundlingByItemSkus(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(this.activePromoBundlings);
    Set<String> activePromoBundlings = this.cacheItemHelperServiceImpl
        .findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, this.pristineDataItem);

    verify(itemRepository).findItemSkusByPristine(STORE_ID, this.pristineDataItem);
    Assertions.assertNotNull(activePromoBundlings);
    Assertions.assertEquals(new HashSet<>(), activePromoBundlings);
  }

  @Test
  public void findCacheableActivePromoBundlingsByStoreIdAndPristine_throwException() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.cacheItemHelperServiceImpl
        .findCacheableActivePromoBundlingsByStoreIdAndPristine(STORE_ID, null));
  }

  @Test
  public void findCacheableActivePromoBundlingsByStoreIdAndItemCode_success(){
    when(itemPickupPointService.getActivePromoBundlingByItemSkus(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(this.activePromoBundlings);
    when(itemRepository.findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_CODE))
        .thenReturn(new HashSet<>(Arrays.asList(ITEM_SKU)));
    Set<String> activePromoBundlings = this.cacheItemHelperServiceImpl
        .findCacheableActivePromoBundlingsByStoreIdAndItemCode(STORE_ID, ITEM_CODE);

    verify(itemPickupPointService).getActivePromoBundlingByItemSkus(eq(STORE_ID), Mockito.anyList());
    verify(itemRepository).findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_CODE);
    Assertions.assertNotNull(activePromoBundlings);
    Assertions.assertEquals(this.activePromoBundlings, activePromoBundlings);
  }

  @Test
  public void findCacheableActivePromoBundlingsByStoreIdAndItemCodeEmptyItemSkusSuccess(){
    when(itemPickupPointService.getActivePromoBundlingByItemSkus(eq(STORE_ID), Mockito.anyList()))
        .thenReturn(this.activePromoBundlings);
    when(itemRepository.findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_CODE))
        .thenReturn(new HashSet<>());
    Set<String> activePromoBundlings = this.cacheItemHelperServiceImpl
        .findCacheableActivePromoBundlingsByStoreIdAndItemCode(STORE_ID, ITEM_CODE);
    verify(itemRepository).findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_CODE);
    Assertions.assertNotNull(activePromoBundlings);
    Assertions.assertEquals(new HashSet<>(), activePromoBundlings);
  }

  @Test
  public void findCacheableActivePromoBundlingsByStoreIdAndItemCode_throwException() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.cacheItemHelperServiceImpl
        .findCacheableActivePromoBundlingsByStoreIdAndItemCode(STORE_ID, null));
  }

  @Test
  public void findCacheableActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode_success(){
    when(itemPickupPointService.getActivePromoBundlingByItemSkusAndPickupPointCode(eq(STORE_ID),
        Mockito.anyList(), eq(PICKUP_POINT_CODE))).thenReturn(this.activePromoBundlings);
    when(itemRepository.findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_CODE))
        .thenReturn(new HashSet<>(Arrays.asList(ITEM_SKU)));
    Set<String> activePromoBundlings = this.cacheItemHelperServiceImpl
        .findCacheableActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE);

    verify(itemPickupPointService).getActivePromoBundlingByItemSkusAndPickupPointCode(eq(STORE_ID),
        Mockito.anyList(), eq(PICKUP_POINT_CODE));
    verify(itemRepository).findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_CODE);
    Assertions.assertNotNull(activePromoBundlings);
    Assertions.assertEquals(this.activePromoBundlings, activePromoBundlings);
  }

  @Test
  public void findCacheableActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCodeSuccess(){
    when(itemRepository.findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_CODE))
        .thenReturn(new HashSet<>());
    Set<String> activePromoBundlings = this.cacheItemHelperServiceImpl
        .findCacheableActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE);
    verify(itemRepository).findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_CODE);
    Assertions.assertNotNull(activePromoBundlings);
    Assertions.assertEquals(new HashSet<>(), activePromoBundlings);
  }

  @Test
  public void findCacheableActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode_throwException() {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.cacheItemHelperServiceImpl.findCacheableActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(
            STORE_ID, null, PICKUP_POINT_CODE));
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    this.pristineDataItem = new PristineDataItem();
    this.pristineDataItem.setPristineId(PRISTINE_ID);

    this.activePromoBundlings = new HashSet<>();
    this.activePromoBundlings.add("COMBO");

    this.item = new Item();
    this.item.setPristineDataItem(this.pristineDataItem);
    this.item.setItemSku(ITEM_SKU);
    this.item.setItemCode(ITEM_CODE);
    this.item.setActivePromoBundlings(this.activePromoBundlings);

    this.itemCodes = new HashSet<>();
    this.itemCodes.add(ITEM_CODE);
  }

  @Test
  public void findItemCodesByPristine_success(){
    when(this.itemRepository.getItemCodesByPristine(STORE_ID, this.pristineDataItem))
        .thenReturn(this.itemCodes);

    Set<String> itemCodes =
        this.cacheItemHelperServiceImpl.findItemCodesByPristine(STORE_ID, this.pristineDataItem);

    verify(this.itemRepository).getItemCodesByPristine(STORE_ID, this.pristineDataItem);

    Assertions.assertNotNull(itemCodes);
    Assertions.assertEquals(this.itemCodes, itemCodes);
  }

  @Test
  public void findItemCodesByPristine_throwException() {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.cacheItemHelperServiceImpl.findItemCodesByPristine(STORE_ID, null));
  }

  @Test
  public void findByStoreIdAndItemCodeAndMarkForDeleteFalse_success(){
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(this.item.getItemSku());
    when(this.itemRepository.findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(itemSkus);
    Set<String> result = this.cacheItemHelperServiceImpl
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);

    verify(this.itemRepository)
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(this.item.getItemSku(), result.iterator().next());
  }

  @Test
  public void findAllByStoreIdAndItemCodeAndMarkForDeleteFalse_success(){
    List<Item> items = new ArrayList<>();
    Item item = new Item();
    item.setItemCode(ITEM_CODE);
    item.setItemSku(ITEM_SKU);
    items.add(item);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    when(this.itemRepository.findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(itemSkus);
    Set<String> result = this.cacheItemHelperServiceImpl
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    verify(this.itemRepository)
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(this.item.getItemSku(), result.iterator().next());
  }

  @Test
  public void findAllByStoreIdAndItemCodeAndMarkForDeleteFalse_fail(){
    when(this.itemRepository.findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(null);
    Set<String> result = this.cacheItemHelperServiceImpl
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    verify(this.itemRepository)
        .findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Assertions.assertNull(result);
  }

  @Test
  public void findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse_success(){
    when(this.pristineItemRepository.findByPristineId(PRISTINE_ID))
        .thenReturn(this.pristineDataItem);
    when(this.itemRepository.getItemCodesByPristine(STORE_ID, this.pristineDataItem))
        .thenReturn(this.itemCodes);

    Set<String> itemCodes = this.cacheItemHelperServiceImpl
        .findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(STORE_ID, PRISTINE_ID);

    verify(this.pristineItemRepository).findByPristineId(PRISTINE_ID);
    verify(this.itemRepository).getItemCodesByPristine(STORE_ID, this.pristineDataItem);
  }

  @Test
  public void findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalseTest() {
    Item item = new Item();
    when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false))
        .thenReturn(item);
    when(applicationContext.getBean(CacheItemHelperService.class)).thenReturn(cacheItemHelperServiceImpl);
    Item result = cacheItemHelperServiceImpl
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemRepository)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    verify(applicationContext).getBean(CacheItemHelperService.class);
    Assertions.assertEquals(result, item);
  }

  @Test
  public void findCacheableByStoreIdDeletedItemTest() {
    Item item = new Item();
    item.setMarkForDelete(true);
    when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false))
        .thenReturn(item);
    when(applicationContext.getBean(CacheItemHelperService.class)).thenReturn(cacheItemHelperServiceImpl);
    Item result = cacheItemHelperServiceImpl
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemRepository)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    verify(applicationContext).getBean(CacheItemHelperService.class);
    Assertions.assertNull(result);
  }

  @Test
  public void findCacheableByStoreIdNullItemTest() {
    when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false))
        .thenReturn(null);
    when(applicationContext.getBean(CacheItemHelperService.class)).thenReturn(cacheItemHelperServiceImpl);
    Item result = cacheItemHelperServiceImpl
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemRepository)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    verify(applicationContext).getBean(CacheItemHelperService.class);
    Assertions.assertNull(result);
  }

  @Test
  public void findCacheableByStoreIdAndItemSku() {
    Item item = new Item();
    when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    Item result = cacheItemHelperServiceImpl.findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    Assertions.assertEquals(result, item);
  }

  @Test
  public void setItemCacheByStoreIdAndItemSku() {
    when(redisTemplate.opsForValue()).thenReturn(valueOperations);
    cacheItemHelperServiceImpl.setItemCacheByStoreIdAndItemSku(STORE_ID, ITEM_SKU, item);
    verify(redisTemplate).opsForValue();
    verify(valueOperations).set(Mockito.anyString(), eq(item));
  }

  @Test
  public void findCacheableByStoreIdAndProductSkuTest() {
    when(itemRepository.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(Arrays.asList(item));
    List<Item> result =
        cacheItemHelperServiceImpl.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemRepository).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Assertions.assertEquals(result, Arrays.asList(item));
  }

  @Test
  public void findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalseTest() {
    when(
        itemRepository.findItemsByStoreIdAndProductSkuAndMarkForDelete(STORE_ID, PRODUCT_SKU, Boolean.FALSE, false))
        .thenReturn(Arrays.asList(item));
    List<Item> result = cacheItemHelperServiceImpl
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemRepository)
        .findItemsByStoreIdAndProductSkuAndMarkForDelete(STORE_ID, PRODUCT_SKU, Boolean.FALSE, false);
    Assertions.assertEquals(result, Arrays.asList(item));
  }

  @Test
  public void findCacheableByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalseTest() {
    when(itemRepository
        .findItemsByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
            false)).thenReturn(Arrays.asList(item));
    List<Item> result = cacheItemHelperServiceImpl
        .findCacheableByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(STORE_ID,
            PRODUCT_SKU, false);
    verify(itemRepository)
        .findItemsByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
            false);
    Assertions.assertEquals(result, Arrays.asList(item));
  }

  @Test
  public void getCacheableItemsByProductSkusTest(){
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    when(redisTemplate.opsForValue())
        .thenReturn(Mockito.mock(ValueOperations.class));
    this.cacheItemHelperServiceImpl.getCacheableItemsByItemSkus(STORE_ID, productSkus);
    this.cacheItemHelperServiceImpl.getCacheableItemsByProductSkus(STORE_ID, productSkus);
    verify(this.redisTemplate, times(2)).opsForValue();
  }

  @Test
  public void getCacheableItemsByProductSkusForSuspensionTest(){
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    when(redisTemplate.opsForValue())
        .thenReturn(Mockito.mock(ValueOperations.class));
    this.cacheItemHelperServiceImpl.getCacheableItemsByProductSkusForSuspensionList(STORE_ID, productSkus);
    this.cacheItemHelperServiceImpl.getCacheableItemsByProductSkusForSuspensionList(STORE_ID, productSkus);
    verify(this.redisTemplate, times(2)).opsForValue();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(itemRepository, pristineItemRepository);
    Mockito.verifyNoMoreInteractions(redisTemplate, valueOperations);
    Mockito.verifyNoMoreInteractions(applicationContext);
    Mockito.verifyNoMoreInteractions(itemPickupPointService);
  }
}
