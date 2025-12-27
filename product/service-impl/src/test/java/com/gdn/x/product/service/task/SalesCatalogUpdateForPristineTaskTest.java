package com.gdn.x.product.service.task;

import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.SaveAndPublishService;

/**
 * Created by Vishal on 25/09/17.
 */
public class SalesCatalogUpdateForPristineTaskTest {

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private ItemService itemService;

  @Mock
  private PristineItemRepository pristineItemRepository;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  private SalesCatalogUpdateForPristineTask task;

  private static final String PRISTINE_ID1 = "pristineId1";
  private static final String PRISTINE_ID2 = "pristineId2";
  private List<String> list;
  private static final String STORE_ID = "storeId";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    list = Arrays.asList(PRISTINE_ID1, PRISTINE_ID2);
    task = new SalesCatalogUpdateForPristineTask(list, itemService, pristineItemRepository, "storeId",
        cacheEvictHelperService, saveAndPublishService);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(
        pristineItemRepository, itemService, cacheEvictHelperService, saveAndPublishService);
  }

  @Test
  public void testCallWhenSucess() throws Exception {
    PristineDataItem pristineDataItem = new PristineDataItem();
    Item item = new Item();
    item.setPristineDataItem(pristineDataItem);
    List<Item> items = new ArrayList<>();
    items.add(item);
    List<ItemCatalogVO> itemCatalogVOS = new ArrayList<>();
    ItemCatalogVO itemCatalogVO = new ItemCatalogVO();
    itemCatalogVOS.add(itemCatalogVO);
    List<SalesCategorySequence> salesCategorySequenceList = new ArrayList<>();
    when(pristineItemRepository.findByPristineId(anyString())).thenReturn(pristineDataItem);
    when(itemService.getItemCatalogsByPristineId(anyString())).thenReturn(itemCatalogVOS);
    when(itemService.getSalesCategorySequenceListFromCategoryHierarchy(anyList()))
        .thenReturn(salesCategorySequenceList);
    when(itemService.getItemsByPristineId(eq(STORE_ID), anyString()))
        .thenReturn(items);
    doNothing().when(saveAndPublishService).publishListOfItems(anyList());
    boolean result = task.call();
    for (String pristineId: list) {
      verify(pristineItemRepository).findByPristineId(pristineId);
      verify(itemService).getItemCatalogsByPristineId(pristineId);
      verify(itemService).getItemsByPristineId(STORE_ID, pristineId);
    }
    verify(cacheEvictHelperService, times(list.size())).evictPristineItemCache(
        STORE_ID, item.getPristineDataItem(), items);
    verify(itemService, times(list.size())).getSalesCategorySequenceListFromCategoryHierarchy(itemCatalogVOS);
    verify(pristineItemRepository, times(list.size())).save(item.getPristineDataItem());
    verify(saveAndPublishService, times(list.size())).publishListOfItems(items);
    Assertions.assertTrue(result);
  }

  @Test
  public void testCall_WhenPristineNotFound() throws Exception{
    PristineDataItem pristineDataItem = new PristineDataItem();
    Item item = new Item();
    item.setPristineDataItem(pristineDataItem);
    List<Item> items = new ArrayList<>();
    items.add(item);
    List<ItemCatalogVO> itemCatalogVOS = new ArrayList<>();
    ItemCatalogVO itemCatalogVO = new ItemCatalogVO();
    itemCatalogVOS.add(itemCatalogVO);
    List<SalesCategorySequence> salesCategorySequenceList = new ArrayList<>();
    when(pristineItemRepository.findByPristineId(anyString())).thenReturn(null);
    when(itemService.getItemCatalogsByPristineId(anyString())).thenReturn(itemCatalogVOS);
    when(itemService.getSalesCategorySequenceListFromCategoryHierarchy(anyList()))
        .thenReturn(salesCategorySequenceList);
    boolean result = task.call();
    for (String pristineId: list) {
      verify(pristineItemRepository).findByPristineId(pristineId);
      verify(itemService).getItemCatalogsByPristineId(pristineId);
    }
    verify(itemService, times(list.size())).getSalesCategorySequenceListFromCategoryHierarchy(itemCatalogVOS);
    Assertions.assertFalse(result);
  }

  @Test
  public void testCallWhenFailed() throws Exception {
    when(itemService.getItemCatalogsByPristineId(anyString())).thenThrow(new RuntimeException());
    boolean result = task.call();
    for (String pristineId: list) {
      verify(itemService).getItemCatalogsByPristineId(pristineId);
    }
    Assertions.assertFalse(result);
  }

}
