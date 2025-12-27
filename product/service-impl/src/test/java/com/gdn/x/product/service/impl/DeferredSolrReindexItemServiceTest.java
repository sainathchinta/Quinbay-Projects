package com.gdn.x.product.service.impl;

import java.util.Arrays;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import java.util.List;

import com.gdn.x.product.dao.api.DeferredSolrReindexItemRepository;
import com.gdn.x.product.model.entity.DeferredSolrReindexItem;


public class DeferredSolrReindexItemServiceTest {

  private static final String ITEM_SKU = "item_sku";
  private static final String STORE_ID = "store_id";

  @Mock
  private DeferredSolrReindexItemRepository deferredSolrReindexItemRepository;

  @InjectMocks
  private DeferredSolrReindexItemServiceImpl deferredSolrReindexItemService;

  private DeferredSolrReindexItem deferredSolrReindexItem;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
    deferredSolrReindexItem = new DeferredSolrReindexItem();
    deferredSolrReindexItem.setItemSku(ITEM_SKU);
  }

  @Test
  public void saveTest() {
    Mockito.when(deferredSolrReindexItemRepository.save(deferredSolrReindexItem)).thenReturn(deferredSolrReindexItem);
    DeferredSolrReindexItem result = deferredSolrReindexItemService.save(deferredSolrReindexItem);
   Assertions.assertEquals(ITEM_SKU, result.getItemSku());
    Mockito.verify(deferredSolrReindexItemRepository).save(deferredSolrReindexItem);
  }

  @Test
  public void saveListTest() {
    Mockito.when(deferredSolrReindexItemRepository.saveAll(Arrays.asList(deferredSolrReindexItem)))
        .thenReturn(Arrays.asList(deferredSolrReindexItem));
    List<DeferredSolrReindexItem> result = deferredSolrReindexItemService.save(Arrays.asList(deferredSolrReindexItem));
   Assertions.assertEquals(ITEM_SKU, result.get(0).getItemSku());
    Mockito.verify(deferredSolrReindexItemRepository).saveAll(Arrays.asList(deferredSolrReindexItem));
  }

  @Test
  public void findByStoreIdTest() {
    Mockito.when(deferredSolrReindexItemRepository
        .findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDate(STORE_ID, PageRequest.of(0, 10)))
        .thenReturn(new PageImpl<>(Arrays.asList(deferredSolrReindexItem), PageRequest.of(0, 10), 1));
    Page<DeferredSolrReindexItem> page = deferredSolrReindexItemService.findByStoreId(STORE_ID, PageRequest.of(0, 10));
   Assertions.assertEquals(ITEM_SKU, page.getContent().get(0).getItemSku());
    Mockito.verify(deferredSolrReindexItemRepository)
        .findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDate(STORE_ID, PageRequest.of(0, 10));
  }

}
