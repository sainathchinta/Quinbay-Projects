package com.gdn.x.product.service.impl;

import java.util.Collections;
import java.util.List;
import java.util.Map;

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

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.api.ItemPickupPointMigrationRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemMigrationStatus;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.ItemPickupPointMigration;
import com.gdn.x.product.model.entity.SystemParameter;

public class ItemPickupPointMigrationServiceImplTest {

  private static final String STATUS = "status";
  private static final int LIMIT = 10;
  private static final String ITEM_SKU = "itemSku";
  private static final String ERROR_MESSAGE = "errorMessage";

  private ItemPickupPointMigration itemPickupPointMigration = new ItemPickupPointMigration();
  private Map<String, String> failedSkuMap = Collections.singletonMap(ITEM_SKU, ERROR_MESSAGE);
  private SystemParameter systemParameter = new SystemParameter();

  @Captor
  private ArgumentCaptor<List<ItemPickupPointMigration>> listArgumentCaptor;

  @Mock
  private ItemPickupPointMigrationRepository itemPickupPointMigrationRepository;

  @Mock
  private SystemParameterServiceImpl systemParameterService;

  @InjectMocks
  private ItemPickupPointMigrationServiceImpl itemPickupPointMigrationService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    systemParameter.setValue(Boolean.TRUE.toString());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(itemPickupPointMigrationRepository);
    Mockito.verifyNoMoreInteractions(systemParameterService);
  }

  @Test
  public void findByStatusAndLimitTest() {
    this.itemPickupPointMigrationService.findByStatusAndLimit(STATUS, LIMIT);
    Mockito.verify(this.itemPickupPointMigrationRepository).getItemsByStatusAndLimit(STATUS, LIMIT);
  }

  @Test
  public void saveCollection_emptyTest() {
    this.itemPickupPointMigrationService.saveCollection(Collections.emptyList());
  }

  @Test
  public void saveCollectionTest() {
    this.itemPickupPointMigrationService.saveCollection(
      Collections.singletonList(itemPickupPointMigration));
    Mockito.verify(this.itemPickupPointMigrationRepository)
      .saveAll(Collections.singletonList(itemPickupPointMigration));
  }

  @Test
  public void findByItemSkuTest() {
    this.itemPickupPointMigrationService.findByItemSku(ITEM_SKU);
    Mockito.verify(this.itemPickupPointMigrationRepository).findByItemSku(ITEM_SKU);
  }

  @Test
  public void findByItemSku_emptyStringTest() {
    this.itemPickupPointMigrationService.findByItemSku(null);
  }

  @Test
  public void updateStatusByItemSkuTest() {
    this.itemPickupPointMigrationService.updateStatusByItemSku(Collections.singletonList(ITEM_SKU),
      STATUS);
    Mockito.verify(this.itemPickupPointMigrationRepository)
      .updateStatusByItemSkus(Collections.singletonList(ITEM_SKU), STATUS);
  }

  @Test
  public void updateFailedStatusByItemSkuTest() {
    itemPickupPointMigration.setItemSku(ITEM_SKU);
    Mockito.when(
        this.itemPickupPointMigrationRepository.findByItemSkuIn(Collections.singleton(ITEM_SKU)))
      .thenReturn(Collections.singletonList(itemPickupPointMigration));
    this.itemPickupPointMigrationService.updateFailedStatusByItemSku(failedSkuMap);
    Mockito.verify(this.itemPickupPointMigrationRepository)
      .findByItemSkuIn(Collections.singleton(ITEM_SKU));
    Mockito.verify(this.itemPickupPointMigrationRepository).saveAll(listArgumentCaptor.capture());
    Assertions.assertEquals(ERROR_MESSAGE, listArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void insertItemSkuByStateTest() {
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
      SystemParameterNames.MIGRATION_ITEM_CREATION_CHANGE_SWITCH)).thenReturn(systemParameter);
    this.itemPickupPointMigrationService.insertItemSkuByState(Collections.singletonList(ITEM_SKU),
      ItemMigrationStatus.PENDING.name());
    Mockito.verify(this.itemPickupPointMigrationRepository).saveAll(listArgumentCaptor.capture());
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterNames.MIGRATION_ITEM_CREATION_CHANGE_SWITCH);
    Assertions.assertEquals(ItemMigrationStatus.PENDING.name(),
      listArgumentCaptor.getValue().get(0).getStatus());
  }

  @Test
  public void insertItemSkuByState_listenerSwitchOffTest() {
    systemParameter.setValue(Boolean.FALSE.toString());
    Mockito.when(
      this.systemParameterService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterNames.MIGRATION_ITEM_CREATION_CHANGE_SWITCH)).thenReturn(systemParameter);
    this.itemPickupPointMigrationService.insertItemSkuByState(Collections.singletonList(ITEM_SKU),
      ItemMigrationStatus.PENDING.name());
    Mockito.verify(this.systemParameterService)
      .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterNames.MIGRATION_ITEM_CREATION_CHANGE_SWITCH);
  }

  @Test
  public void insertItemSkuByState_exceptionTest() {
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
      SystemParameterNames.MIGRATION_ITEM_CREATION_CHANGE_SWITCH)).thenReturn(systemParameter);
    Mockito.doThrow(RuntimeException.class).when(this.itemPickupPointMigrationRepository)
      .saveAll(Mockito.anyList());
    this.itemPickupPointMigrationService.insertItemSkuByState(Collections.singletonList(ITEM_SKU),
      ItemMigrationStatus.PENDING.name());
    Mockito.verify(this.systemParameterService)
      .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterNames.MIGRATION_ITEM_CREATION_CHANGE_SWITCH);
    Mockito.verify(this.itemPickupPointMigrationRepository).saveAll(listArgumentCaptor.capture());
  }

  @Test
  public void insertItemSkuByState_exceptionSystemParameterTest() {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.systemParameterService)
      .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterNames.MIGRATION_ITEM_CREATION_CHANGE_SWITCH);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointMigrationService.insertItemSkuByState(Collections.singletonList(ITEM_SKU),
        ItemMigrationStatus.PENDING.name()));
    } finally {
      Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
          SystemParameterNames.MIGRATION_ITEM_CREATION_CHANGE_SWITCH);
    }
  }
}