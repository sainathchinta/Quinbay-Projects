package com.gdn.x.product.service.impl;

import static org.mockito.ArgumentMatchers.eq;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
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
import org.springframework.data.domain.Pageable;

import com.gdn.x.product.dao.api.DeferredSolrReindexItemRepository;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.model.entity.DeferredSolrReindexItem;

public class DeferredSolrReindexItemServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final Pageable PAGEABLE =PageRequest.of(0, 3);

  @InjectMocks
  private DeferredSolrReindexItemServiceImpl deferredSolrReindexItemServiceImpl;

  @Mock
  private DeferredSolrReindexItemRepository deferredSolrReindexItemRepository;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  public void saveTest(){
    DeferredSolrReindexItem deferredSolrReindexItem = new DeferredSolrReindexItem();
    Mockito.when(deferredSolrReindexItemRepository.save(deferredSolrReindexItem)).thenReturn(deferredSolrReindexItem);
    this.deferredSolrReindexItemServiceImpl.save(deferredSolrReindexItem);
    Mockito.verify(this.deferredSolrReindexItemRepository).save(deferredSolrReindexItem);
  }

  @Test
  public void saveListTest() {
    DeferredSolrReindexItem deferredSolrReindexItem = new DeferredSolrReindexItem();
    Mockito.when(deferredSolrReindexItemRepository.saveAll(Arrays.asList(deferredSolrReindexItem)))
        .thenReturn(Arrays.asList(deferredSolrReindexItem));
    this.deferredSolrReindexItemServiceImpl.save(Arrays.asList(deferredSolrReindexItem));
    Mockito.verify(this.deferredSolrReindexItemRepository).saveAll(Arrays.asList(deferredSolrReindexItem));
  }

  @Test
  public void findByStoreIdTest() {
    Mockito.when(
        deferredSolrReindexItemRepository.findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDate(STORE_ID, PAGEABLE))
        .thenReturn(new PageImpl<>(Arrays.asList(new DeferredSolrReindexItem())));
    Page<DeferredSolrReindexItem> response = this.deferredSolrReindexItemServiceImpl.findByStoreId(STORE_ID, PAGEABLE);
    Mockito.verify(this.deferredSolrReindexItemRepository)
        .findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDate(STORE_ID, PAGEABLE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.getContent().size());
  }

  @Test
  public void findByStoreIdAndItemSkuInTest() {
    Mockito.when(deferredSolrReindexItemRepository.findByStoreIdAndItemSkuIn(STORE_ID, Arrays.asList(STORE_ID)))
        .thenReturn(Arrays.asList(new DeferredSolrReindexItem()));
    List<DeferredSolrReindexItem> response =
        this.deferredSolrReindexItemServiceImpl.findByStoreIdAndItemSkuIn(STORE_ID, Arrays.asList(STORE_ID));
    Mockito.verify(this.deferredSolrReindexItemRepository)
        .findByStoreIdAndItemSkuIn(STORE_ID, Arrays.asList(STORE_ID));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.size());
  }

  @Test
  public void findByStoreIdAndReindexTypeAndProductReindexStatusTest() {
    Mockito.when(
        deferredSolrReindexItemRepository.findByStoreIdAndReindexTypeAndProductReindexStatusAndMarkForDeleteFalse(
            eq(STORE_ID), eq(ReindexType.ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING),
            Mockito.any())).thenReturn(Arrays.asList(new DeferredSolrReindexItem()));
    List<DeferredSolrReindexItem> response =
        this.deferredSolrReindexItemServiceImpl.findByStoreIdAndReindexTypeAndProductReindexStatus(STORE_ID,
            ReindexType.ITEM_REINDEX.getDescription(), ProductReindexStatus.REINDEX_PENDING,PageRequest.of(0, 10));
    Mockito.verify(this.deferredSolrReindexItemRepository)
        .findByStoreIdAndReindexTypeAndProductReindexStatusAndMarkForDeleteFalse(eq(STORE_ID),
            eq(ReindexType.ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING), Mockito.any());
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.size());
  }


  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.deferredSolrReindexItemRepository);
  }
}
