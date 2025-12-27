package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.product.dao.api.ItemArchiveRepository;
import com.gdn.x.product.dao.api.ItemPickupPointArchiveRepository;
import com.gdn.x.product.dao.api.ProductArchiveRepository;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.ProductArchive;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.service.api.SystemParameterService;

public class ProductDataArchivalServiceTest {

  private static final String STORE_ID = "10001";
  private static final String PRODUCT_SKU = "productSku";


  @InjectMocks
  private ProductDataArchivalServiceImpl productDataArchivalService;

  @Mock
  private ProductArchiveRepository productArchiveRepository;

  @Mock
  private ItemArchiveRepository itemArchiveRepository;

  @Mock
  private ItemPickupPointArchiveRepository itemPickupPointArchiveRepository;

  @Mock
  private SystemParameterService systemParameterService;

  private ProductArchive product;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);

    product = new ProductArchive();
    product.setProductSku(PRODUCT_SKU);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productArchiveRepository, itemArchiveRepository, itemPickupPointArchiveRepository,
        systemParameterService);
  }

  @Test
  public void deleteProductArchivedDataTest() {
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS, "1",
            SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION, "1",
            SystemParameterNames.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION));
    Mockito.when(productArchiveRepository.fetchArchivedListOfProduct(Mockito.any(Date.class), Mockito.eq(1)))
        .thenReturn(Arrays.asList(product));

    productDataArchivalService.deleteProductArchivedData(STORE_ID);

    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION);
    Mockito.verify(productArchiveRepository).fetchArchivedListOfProduct(Mockito.any(Date.class), Mockito.eq(1));
    Mockito.verify(productArchiveRepository).deleteByProductSku(Arrays.asList(PRODUCT_SKU));
    Mockito.verify(itemArchiveRepository).deleteByProductSku(Arrays.asList(PRODUCT_SKU));
    Mockito.verify(itemPickupPointArchiveRepository).deleteByProductSku(Arrays.asList(PRODUCT_SKU));
  }

  @Test
  public void deleteProductArchivedDataNoProductTest() {
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS, "1",
            SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION, "1",
            SystemParameterNames.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION));
    Mockito.when(productArchiveRepository.fetchArchivedListOfProduct(Mockito.any(Date.class), Mockito.eq(1)))
        .thenReturn(new ArrayList<>());

    productDataArchivalService.deleteProductArchivedData(STORE_ID);

    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION);
    Mockito.verify(productArchiveRepository).fetchArchivedListOfProduct(Mockito.any(Date.class), Mockito.eq(1));
  }

  @Test
  public void deleteProductArchivedDataBatchSize0Test() {
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS, "0",
            SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS));

    productDataArchivalService.deleteProductArchivedData(STORE_ID);

    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS);
  }
}
