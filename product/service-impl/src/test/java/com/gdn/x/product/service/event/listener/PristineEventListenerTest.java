package com.gdn.x.product.service.event.listener;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.HashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.ext.catalog.model.entity.BlibliApprovedProductChange;
import com.gdn.x.product.domain.event.model.PristineDataItemEventModel;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.service.api.CacheEvictItemService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.PristineCacheableService;
import com.gdn.x.product.service.api.SystemParameterService;

/**
 * Created by govind on 13/09/2017 AD.
 */
public class PristineEventListenerTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "10001";
  private static final String PRISTINE_ID = "pristineId";
  private static final String PRISTINE_MASTER_ID = "pristineMasterId";
  private static final String BRAND = "brand";
  private static final String PRODUCT_NAME = "productName";
  private static final String HANDPHONE = "HANDPHONE";
  private static final String ITEM_CODE = "ITEM_CODE";
  private static final String MESSAGE = "message";

  private BlibliApprovedProductChange message;

  @InjectMocks
  private PristineEventListener pristineEventListener;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemService itemService;

  @Mock
  private PristineCacheableService pristineCacheableService;

  @Mock
  private CacheEvictItemService cacheEvictItemService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    message = new BlibliApprovedProductChange();
    ReflectionTestUtils.setField(pristineEventListener, "isPristineSettingEnabled", true);
    ReflectionTestUtils.setField(pristineEventListener, "storeId", "10001");
    PristineDataItemEventModel pristineDataItemEventModel = new PristineDataItemEventModel();
    pristineDataItemEventModel.setPristineId(PRISTINE_ID);
    pristineDataItemEventModel.setPristineMasterId(PRISTINE_MASTER_ID);
    pristineDataItemEventModel.setPristineCategory(HANDPHONE);
    pristineDataItemEventModel.setPristineBrand(BRAND);
    pristineDataItemEventModel.setPristineProductName(PRODUCT_NAME);
    pristineDataItemEventModel.setPcbProductItemId(ITEM_CODE);
    pristineDataItemEventModel.setPristineListingAttributes(new HashMap<String, String>());
    message.setPristineDataItemEventModel(pristineDataItemEventModel);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.pristineCacheableService, this.systemParameterService);
    verifyNoMoreInteractions(this.objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {

    message.setProductId(PRODUCT_CODE);
    doNothing().when(itemService)
        .updateItemsPristineDataByItemCode(Mockito.any(PristineDataItem.class));
    doNothing().when(pristineCacheableService)
        .evictPristineItemAndSiblingsCacheAndRebuild(STORE_ID, Mockito.mock(PristineDataItem.class));
    doNothing().when(cacheEvictItemService)
        .evictFindItemByItemSku(anyString(), ArgumentMatchers.eq(PRISTINE_ID));
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED,
            "false", ""));
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlibliApprovedProductChange.class))
      .thenReturn(message);
    this.pristineEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlibliApprovedProductChange.class);
    verify(itemService).updateItemsPristineDataByItemCode(Mockito.any(PristineDataItem.class));
    verify(pristineCacheableService)
         .evictPristineItemAndSiblingsCacheAndRebuild(ArgumentMatchers.eq(STORE_ID), Mockito.any(PristineDataItem.class));
    verify(cacheEvictItemService)
        .evictFindItemSkusByPristineId(ArgumentMatchers.eq(STORE_ID), ArgumentMatchers.eq(PRISTINE_ID));
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED);
  }

  @Test
  public void onDomainEventConsumed_isPristineSettingEnabledFalseTest() throws Exception {
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED,
            "false", ""));
    ReflectionTestUtils.setField(pristineEventListener, "isPristineSettingEnabled", false);
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlibliApprovedProductChange.class))
      .thenReturn(message);
    this.pristineEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlibliApprovedProductChange.class);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID,SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED);

  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    message.setProductId(PRODUCT_CODE);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED,
            "false", ""));
    doThrow(RuntimeException.class).when(this.itemService)
        .updateItemsPristineDataByItemCode(Mockito.any(PristineDataItem.class));
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlibliApprovedProductChange.class))
      .thenReturn(message);
    this.pristineEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlibliApprovedProductChange.class);
    verify(itemService).updateItemsPristineDataByItemCode(Mockito.any(PristineDataItem.class));
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED);

  }

  @Test
  public void onDomainEventConsumed_WhenItemCodeNullTest() throws Exception {

    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED,
            "false", ""));
    message.setPristineDataItemEventModel(null);
    message.setProductId(PRODUCT_CODE);
    doNothing().when(itemService)
        .updateItemsPristineDataByItemCode(Mockito.any(PristineDataItem.class));
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlibliApprovedProductChange.class))
      .thenReturn(message);
    this.pristineEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlibliApprovedProductChange.class);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED);
  }

  @Test
  public void onDomainEventConsumed_WhenPVTestingModeTrueTest() throws Exception {
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED,
            "true", ""));
    Mockito.when(this.objectMapper.readValue(MESSAGE, BlibliApprovedProductChange.class))
      .thenReturn(message);
    this.pristineEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BlibliApprovedProductChange.class);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRISTINE_SETTINGS_TESTING_MODE_ENABLED);
  }
}
