package com.gdn.x.product.service.cache;

import static org.mockito.Mockito.when;

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
import org.mockito.MockitoAnnotations;
import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.vo.CacheItemVO;
import com.gdn.x.product.model.vo.PristineItemAndSiblingsVO;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.PristineCacheableService;

public class PristineCacheableServiceImplTest {

  private static final String STORE_ID = "10001";

  private static final String PRISTINE_ID = "PI-001-001";
  private static final String PRISTINE_MASTER_ID = "pristineMasterId";

  private static final String ITEM_ID = "itemId";

  private static final String ITEM_SKU = "item_sku";

  @InjectMocks
  private PristineCacheableServiceImpl pristineCacheableService;

  @Mock
  private PristineItemRepository pristineItemRepository;

  @Mock
  private ItemRepository itemRepository;
  @Mock
  private ItemService itemService;

  @Mock
  private RedisTemplate stringRedisTemplate;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private ApplicationContext applicationContext;

  private List<Item> items;


  @Test
  public void findPristineItemAndItsSiblingsByPristineId_WithoutData() {
    this.pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(
        STORE_ID, PRISTINE_ID);
    Mockito.verify(this.pristineItemRepository).findByPristineId(PRISTINE_ID);
  }

  @Test
  public void findPristineItemAndItsSiblingsByPristineId_WithData() {
    List<PristineDataItem> siblings = new ArrayList<>();
    PristineDataItem sibling = new PristineDataItem();
    siblings.add(sibling);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineMasterId(PRISTINE_MASTER_ID);
    pristineDataItem.setProductCondition("NEW");
    when(pristineItemRepository.findByPristineId(PRISTINE_ID)).thenReturn(pristineDataItem);
    when( pristineItemRepository.findByPristineMasterIdAndProductConditionAndPristineIdNot(
        pristineDataItem.getPristineMasterId(), pristineDataItem.getProductCondition(),
        PRISTINE_ID))
        .thenReturn(siblings);
    PristineItemAndSiblingsVO result = pristineCacheableService
        .findPristineItemAndItsSiblingsByPristineId(STORE_ID, PRISTINE_ID);
    Assertions.assertEquals(pristineDataItem, result.getPristineDataItem());
    Assertions.assertEquals(siblings, result.getSiblings());
    Mockito.verify(this.pristineItemRepository).findByPristineId(PRISTINE_ID);
    Mockito.verify(this.pristineItemRepository)
        .findByPristineMasterIdAndProductConditionAndPristineIdNot(
            pristineDataItem.getPristineMasterId(), pristineDataItem.getProductCondition(),
            PRISTINE_ID);
  }

  @Test
  public void findItemByPristine_WithoutData(){
    PristineDataItem pristineDataItem = Mockito.mock(PristineDataItem.class);
    when(this.cacheItemHelperService
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID,
            pristineDataItem.getPristineId())).thenReturn(null);
    this.pristineCacheableService.findItemByPristine(STORE_ID, pristineDataItem);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID,
            pristineDataItem.getPristineId());
  }

  @Test
  public void findItemByPristine_WithData(){
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    Mockito.when(
            itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus))
        .thenReturn(Arrays.asList(item));
    PristineDataItem pristineDataItem = Mockito.mock(PristineDataItem.class);
    when(this.cacheItemHelperService
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID,
            pristineDataItem.getPristineId())).thenReturn(itemSkus);
    CacheItemVO result = this.pristineCacheableService.findItemByPristine(STORE_ID, pristineDataItem);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID,
            pristineDataItem.getPristineId());
    Mockito.verify(itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus);
  }

  @Test
  public void findItemByPristine_WhenItemCodeNotExist(){
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    items.get(0).setItemCode(null);
    PristineDataItem pristineDataItem = Mockito.mock(PristineDataItem.class);
    when(this.cacheItemHelperService
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID,
            pristineDataItem.getPristineId())).thenReturn(itemSkus);
    Mockito.when(
            itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus))
        .thenReturn(Arrays.asList(item));
    CacheItemVO result = this.pristineCacheableService.findItemByPristine(STORE_ID, pristineDataItem);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID,
            pristineDataItem.getPristineId());
    Mockito.verify(itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus);
    Assertions.assertNull(result.getItemCode());
  }

  @Test
  public void evictPristineItemCache_KeyPresent(){
    when(this.stringRedisTemplate.hasKey(Mockito.anyString())).thenReturn(true);
    this.pristineCacheableService.evictPristineItemCache(STORE_ID, PRISTINE_ID);
    Mockito.verify(this.stringRedisTemplate).delete(Mockito.anyString());
  }

  @Test
  public void evictPristineItemCache_KeyAbsent(){
    this.pristineCacheableService.evictPristineItemCache(STORE_ID, PRISTINE_ID);
    Mockito.verify(this.stringRedisTemplate).delete(Mockito.anyString());
  }

  @Test
  public void evictPristineItemAndSiblingsCacheAndRebuild_NoSiblings(){
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineMasterId(PRISTINE_MASTER_ID);
    pristineDataItem.setProductCondition("NEW");
    when(applicationContext.getBean(PristineCacheableService.class)).thenReturn(this.pristineCacheableService);
    when(this.stringRedisTemplate.hasKey(getPristineSiblingsCacheKey(pristineDataItem.getPristineId()))).thenReturn(true);
    when(this.pristineItemRepository
        .findByPristineMasterIdAndProductConditionAndPristineIdNot(
            pristineDataItem.getPristineMasterId(), pristineDataItem.getProductCondition(), pristineDataItem.getPristineId()))
        .thenReturn(new ArrayList<>());
    this.pristineCacheableService.evictPristineItemAndSiblingsCacheAndRebuild(STORE_ID, pristineDataItem);
    Mockito.verify(this.pristineItemRepository)
            .findByPristineMasterIdAndProductConditionAndPristineIdNot(
                pristineDataItem.getPristineMasterId(), pristineDataItem.getProductCondition(), pristineDataItem.getPristineId());
    Mockito.verify(this.stringRedisTemplate).delete(getPristineSiblingsCacheKey(pristineDataItem.getPristineId()));
    Mockito.verify(this.pristineItemRepository).findByPristineId(pristineDataItem.getPristineId());
  }

  /*@Test
  public void evictPristineItemAndSiblingsCacheAndRebuild_WithSiblings() {
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineMasterId(PRISTINE_MASTER_ID);
    pristineDataItem.setProductCondition("NEW");
    List<PristineDataItem> siblings = new ArrayList<>();
    PristineDataItem sibling1 = new PristineDataItem();
    sibling1.setPristineId("SIB-001");
    siblings.add(sibling1);
    List<PristineDataItem> siblingsOfSiblings = new ArrayList<>();
    siblingsOfSiblings.add(pristineDataItem);
    when(applicationContext.getBean(PristineCacheableService.class)).thenReturn(this.pristineCacheableService);
    when(this.stringRedisTemplate.hasKey(Mockito.anyString())).thenReturn(true);
    when(this.pristineItemRepository.findByPristineId("SIB-001")).thenReturn(siblingsOfSiblings.get(0));
    when(this.pristineItemRepository
        .findByPristineMasterIdAndProductConditionAndPristineIdNot(pristineDataItem.getPristineMasterId(),
            pristineDataItem.getProductCondition(), pristineDataItem.getPristineId())).thenReturn(siblings);
    this.pristineCacheableService.evictPristineItemAndSiblingsCacheAndRebuild(STORE_ID, pristineDataItem);
    Mockito.verify(this.pristineItemRepository)
        .findByPristineMasterIdAndProductConditionAndPristineIdNot(pristineDataItem.getPristineMasterId(),
            pristineDataItem.getProductCondition(), pristineDataItem.getPristineId());
    Mockito.verify(this.stringRedisTemplate).hasKey(Mockito.anyString());
    Mockito.verify(this.stringRedisTemplate).delete(getPristineSiblingsCacheKey(pristineDataItem.getPristineId()));
    Mockito.verify(this.pristineItemRepository).findByPristineId(pristineDataItem.getPristineId());
    Mockito.verify(this.stringRedisTemplate).delete(getPristineSiblingsCacheKey(sibling1.getPristineId()));
    Mockito.verify(this.pristineItemRepository)
        .findByPristineMasterIdAndProductConditionAndPristineIdNot(sibling1.getPristineMasterId(),
            sibling1.getProductCondition(), sibling1.getPristineId());
    Mockito.verify(this.pristineItemRepository).findByPristineId(sibling1.getPristineId());
  }*/

  private String getPristineSiblingsCacheKey(String pristineId){
    return ReflectionTestUtils.invokeMethod(
        this.pristineCacheableService, "getPristineSiblingsCacheKey", STORE_ID, pristineId);
  }

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    items = new ArrayList<>();
    Item item1 = new Item();
    item1.setItemCode(ITEM_ID);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    item1.setPristineDataItem(pristineDataItem);
    items.add(item1);
  }

  @Test
  public void findFirstItemByPristine_WithoutData() throws Exception {
    PristineDataItem pristineDataItem = Mockito.mock(PristineDataItem.class);
    when(this.cacheItemHelperService
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID, pristineDataItem.getPristineId()))
        .thenReturn(null);
    Item result = this.pristineCacheableService.findFirstItemByPristine(pristineDataItem);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID, pristineDataItem.getPristineId());
    Assertions.assertNull(result);
  }

  @Test
  public void findFirstItemByPristine_WithData() throws Exception{
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineMasterId(PRISTINE_MASTER_ID);
    pristineDataItem.setProductCondition("NEW");
    when(this.cacheItemHelperService
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID,
            pristineDataItem.getPristineId())).thenReturn(itemSkus);
    Mockito.when(
            itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus))
        .thenReturn(Arrays.asList(item));
    Item result = this.pristineCacheableService.findFirstItemByPristine(pristineDataItem);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID,
            pristineDataItem.getPristineId());
    Mockito.verify(itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus);
  }

  @Test
  public void findFirstItemByPristine_WhenItemCodeNotExist() throws Exception{
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    PristineDataItem pristineDataItem = Mockito.mock(PristineDataItem.class);
    when(this.cacheItemHelperService
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID,
            pristineDataItem.getPristineId())).thenReturn(itemSkus);
    Mockito.when(
            itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus))
        .thenReturn(Arrays.asList(item));
    Item result = this.pristineCacheableService.findFirstItemByPristine(pristineDataItem);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(STORE_ID,
            pristineDataItem.getPristineId());
    Mockito.verify(itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus);
    Assertions.assertNull(result.getItemCode());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.pristineItemRepository);
    Mockito.verifyNoMoreInteractions(this.itemRepository);
  }
}
