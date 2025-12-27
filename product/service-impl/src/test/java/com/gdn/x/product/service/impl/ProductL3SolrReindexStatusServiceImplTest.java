package com.gdn.x.product.service.impl;


import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.x.product.dao.api.ProductL3SolrReindexStatusRepository;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.model.entity.ProductL3SolrReindexStatus;

public class ProductL3SolrReindexStatusServiceImplTest {

  public static final String STORE_ID = "10001";
  @InjectMocks
  private ProductL3SolrReindexStatusServiceImpl productL3SolrReindexStatusService;

  @Mock
  private ProductL3SolrReindexStatusRepository productL3SolrReindexStatusRepository;

  @Captor
  private ArgumentCaptor<ProductL3SolrReindexStatus> productL3SolrReindexStatusArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductL3SolrReindexStatus>> productL3SolrReindexStatusesArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productL3SolrReindexStatusRepository);
  }

  @Test
  public void insertProductSkuToReindexStatusCollection() {
    Mockito
        .when(productL3SolrReindexStatusRepository.findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(null);
    this.productL3SolrReindexStatusService.insertProductSkuToReindexStatusCollection(new ProductL3SolrReindexStatus());
    Mockito.verify(productL3SolrReindexStatusRepository).findByStoreIdAndProductSku(Mockito.isNull(), Mockito.isNull());
    Mockito.verify(productL3SolrReindexStatusRepository).save(productL3SolrReindexStatusArgumentCaptor.capture());
  }

  @Test
  public void insertProductSkuToReindexStatusCollectionNonNullDbResponse() {
    ProductL3SolrReindexStatus productL3SolrReindexStatus = new ProductL3SolrReindexStatus();
    productL3SolrReindexStatus.setId(SolrConstants.STORE_ID);
    productL3SolrReindexStatus.setProductReindexStatus(ProductReindexStatus.REINDEX_PENDING);
    ProductL3SolrReindexStatus productL3SolrReindexStatus1 = new ProductL3SolrReindexStatus();
    productL3SolrReindexStatus1.setProductReindexStatus(ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS);
    Mockito
        .when(productL3SolrReindexStatusRepository.findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(productL3SolrReindexStatus);
    this.productL3SolrReindexStatusService.insertProductSkuToReindexStatusCollection(productL3SolrReindexStatus1);
    Mockito.verify(productL3SolrReindexStatusRepository).findByStoreIdAndProductSku(Mockito.isNull(), Mockito.isNull());
    Mockito.verify(productL3SolrReindexStatusRepository).save(productL3SolrReindexStatusArgumentCaptor.capture());
    Assertions.assertEquals(ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS,
        productL3SolrReindexStatusArgumentCaptor.getValue().getProductReindexStatus());
  }

  @Test
  public void insertProductSkusToReindexStatusCollection() {
    Mockito
        .when(productL3SolrReindexStatusRepository.findByStoreIdAndProductSkuIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(new ArrayList());
    this.productL3SolrReindexStatusService.insertProductSkusToReindexStatusCollection(STORE_ID,
        new ArrayList<>(Arrays.asList(new ProductL3SolrReindexStatus())));
    Mockito.verify(productL3SolrReindexStatusRepository).findByStoreIdAndProductSkuIn(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productL3SolrReindexStatusRepository).saveAll(productL3SolrReindexStatusesArgumentCaptor.capture());
  }

  @Test
  public void insertProductSkusToReindexStatusCollectionNonNullDbResponse() {
    ProductL3SolrReindexStatus productL3SolrReindexStatus = new ProductL3SolrReindexStatus();
    productL3SolrReindexStatus.setId(SolrConstants.STORE_ID);
    productL3SolrReindexStatus.setProductReindexStatus(ProductReindexStatus.REINDEX_PENDING);
    ProductL3SolrReindexStatus productL3SolrReindexStatus1 = new ProductL3SolrReindexStatus();
    productL3SolrReindexStatus1.setProductReindexStatus(ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS);
    Mockito
        .when(productL3SolrReindexStatusRepository.findByStoreIdAndProductSkuIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(new ArrayList(Arrays.asList(productL3SolrReindexStatus)));
    this.productL3SolrReindexStatusService.insertProductSkusToReindexStatusCollection(STORE_ID,
        new ArrayList<>(Arrays.asList(productL3SolrReindexStatus1)));
    Mockito.verify(productL3SolrReindexStatusRepository).findByStoreIdAndProductSkuIn(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productL3SolrReindexStatusRepository).saveAll(productL3SolrReindexStatusesArgumentCaptor.capture());
    Assertions.assertEquals(ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS,
        productL3SolrReindexStatusesArgumentCaptor.getValue().get(0).getProductReindexStatus());
    Assertions.assertEquals(1,
        productL3SolrReindexStatusesArgumentCaptor.getValue().size());
  }

  @Test
  public void insertProductSkusToReindexStatusCollectionUpdatedList0() {
    ProductL3SolrReindexStatus productL3SolrReindexStatus = new ProductL3SolrReindexStatus();
    productL3SolrReindexStatus.setId(SolrConstants.STORE_ID);
    productL3SolrReindexStatus.setProductReindexStatus(ProductReindexStatus.REINDEX_PENDING);
    ProductL3SolrReindexStatus productL3SolrReindexStatus1 = new ProductL3SolrReindexStatus();
    productL3SolrReindexStatus1.setProductReindexStatus(ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS);
    Mockito
        .when(productL3SolrReindexStatusRepository.findByStoreIdAndProductSkuIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(new ArrayList(Arrays.asList(productL3SolrReindexStatus)));
    this.productL3SolrReindexStatusService.insertProductSkusToReindexStatusCollection(STORE_ID,
        new ArrayList<>());
    Mockito.verify(productL3SolrReindexStatusRepository).findByStoreIdAndProductSkuIn(Mockito.anyString(), Mockito.anyList());
  }
}