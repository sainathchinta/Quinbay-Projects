package com.gdn.partners.pbp.service.mv;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;
import com.gdn.partners.pbp.entity.mv.MerchantProductMVIndexingFailed;
import com.gdn.partners.pbp.repository.eventstore.MerchantProductMVIndexingFailedRepository;
import com.gdn.partners.pbp.repository.mv.MerchantProductMVRepository;
import com.gdn.partners.pbp.service.mv.updater.MerchantProductMVProductItemUpdaterServiceBean;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemViewConfig;
import com.gdn.x.product.domain.event.model.MasterDataItem;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;

public class MerchantProductMVProductItemUpdaterServiceBeanTest {

  @Mock
  MerchantProductMVRepository merchantProductMVRepository;

  @Mock
  ProductLevel3Repository productLevel3Repository;

  @Mock
  MerchantProductMVIndexingFailedRepository merchantProductMVIndexingFailedRepository;

  @InjectMocks
  MerchantProductMVProductItemUpdaterServiceBean service;

  ItemChange eventModel;

  MerchantProductMV savedMv;

  private long now_milis;
  private long yesterday_milis;

  private Calendar DATE_NOW;
  private Calendar DATE_YESTERDAY;

  private static final String STORE_ID = "10001";
  private static final String ITEM_SKU = "BLI-000011-000026-011010";

  private ItemSummaryResponse itemSummaryResponse;

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);

    eventModel = new ItemChange();
    savedMv = new MerchantProductMV();

    DATE_NOW = Calendar.getInstance();
    now_milis = DATE_NOW.getTimeInMillis();
    DATE_YESTERDAY = Calendar.getInstance();
    DATE_YESTERDAY.set(Calendar.DATE, Calendar.getInstance().get(Calendar.DATE) - 1);
    yesterday_milis = DATE_YESTERDAY.getTimeInMillis();

    MasterDataItem masterDataItem = new MasterDataItem();
    eventModel.setMasterDataItem(masterDataItem);

    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(ChannelName.DEFAULT.name());
    itemViewConfigs.add(itemViewConfig);

    eventModel.setStoreId(STORE_ID);
    eventModel.setItemSku(ITEM_SKU);
    eventModel.setItemViewConfigs(itemViewConfigs);

    itemSummaryResponse = new ItemSummaryResponse();

    Set<ItemViewConfigDTO> itemViewConfigDtos = new HashSet<>();
    ItemViewConfigDTO itemViewConfigDto = new ItemViewConfigDTO();
    itemViewConfigDto.setChannel(ChannelName.DEFAULT.name());
    itemViewConfigDtos.add(itemViewConfigDto);

    MasterCatalogDTO masterCatalog = new MasterCatalogDTO();
    masterCatalog.setCategory(new CategoryDTO());
    itemSummaryResponse.setItemViewConfigs(itemViewConfigDtos);
    itemSummaryResponse.setMasterCatalog(masterCatalog);
  }

  @Test
  public void update_NewItem() throws Exception {
    Mockito.when(
        merchantProductMVRepository.findByStoreIdAndItemSku(Mockito.eq(eventModel.getStoreId()),
            Mockito.eq(eventModel.getItemSku()))).thenReturn(null);
    Mockito.when(productLevel3Repository.findSummaryByGdnSku(Mockito.eq(eventModel.getItemSku())))
        .thenReturn(itemSummaryResponse);
    service.update(eventModel);
    Mockito.verify(merchantProductMVRepository).findByStoreIdAndItemSku(
        Mockito.eq(eventModel.getStoreId()), Mockito.eq(eventModel.getItemSku()));
    Mockito.verify(productLevel3Repository)
        .findSummaryByGdnSku(Mockito.eq(eventModel.getItemSku()));
    Mockito.verify(merchantProductMVRepository).saveAndFlush(Mockito.any(MerchantProductMV.class));
  }

  @Test
  public void update_NewItem_ErrorOnXProduct_AddToFailedItemSkus() throws Exception {
    Mockito.when(
        merchantProductMVRepository.findByStoreIdAndItemSku(Mockito.eq(eventModel.getStoreId()),
            Mockito.eq(eventModel.getItemSku()))).thenReturn(null);
    Mockito.when(productLevel3Repository.findSummaryByGdnSku(Mockito.eq(eventModel.getItemSku())))
        .thenThrow(
            new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
                "Test failed to get item sku from xproduct"));
    Mockito
        .when(
            merchantProductMVIndexingFailedRepository.findByItemSku(Mockito.eq(eventModel
                .getItemSku()))).thenReturn(null);
    service.update(eventModel);
    Mockito.verify(merchantProductMVRepository).findByStoreIdAndItemSku(
        Mockito.eq(eventModel.getStoreId()), Mockito.eq(eventModel.getItemSku()));
    Mockito.verify(productLevel3Repository)
        .findSummaryByGdnSku(Mockito.eq(eventModel.getItemSku()));
    Mockito.verify(merchantProductMVIndexingFailedRepository).findByItemSku(
        Mockito.eq(eventModel.getItemSku()));
    Mockito.verify(merchantProductMVIndexingFailedRepository).save(
        Mockito.any(MerchantProductMVIndexingFailed.class));
  }

  @Test
  public void update_NewItem_ErrorOnXProduct_NotSavingToFailedItemSkus() throws Exception {
    Mockito.when(
        merchantProductMVRepository.findByStoreIdAndItemSku(Mockito.eq(eventModel.getStoreId()),
            Mockito.eq(eventModel.getItemSku()))).thenReturn(null);
    Mockito.when(productLevel3Repository.findSummaryByGdnSku(Mockito.eq(eventModel.getItemSku())))
        .thenThrow(
            new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
                "Test failed to get item sku from xproduct"));
    Mockito
        .when(
            merchantProductMVIndexingFailedRepository.findByItemSku(Mockito.eq(eventModel
                .getItemSku()))).thenReturn(new MerchantProductMVIndexingFailed());
    service.update(eventModel);
    Mockito.verify(merchantProductMVRepository).findByStoreIdAndItemSku(
        Mockito.eq(eventModel.getStoreId()), Mockito.eq(eventModel.getItemSku()));
    Mockito.verify(productLevel3Repository)
        .findSummaryByGdnSku(Mockito.eq(eventModel.getItemSku()));
    Mockito.verify(merchantProductMVIndexingFailedRepository).findByItemSku(
        Mockito.eq(eventModel.getItemSku()));
  }

  @Test
  public void update_NewItem_ErrorOnXProduct_FailedWhenSavingToFailedItemSkus() throws Exception {
    Mockito.when(
        merchantProductMVRepository.findByStoreIdAndItemSku(Mockito.eq(eventModel.getStoreId()),
            Mockito.eq(eventModel.getItemSku()))).thenReturn(null);
    Mockito.when(productLevel3Repository.findSummaryByGdnSku(Mockito.eq(eventModel.getItemSku())))
        .thenThrow(
            new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
                "Test failed to get item sku from xproduct"));
    Mockito
        .when(
            merchantProductMVIndexingFailedRepository.findByItemSku(Mockito.eq(eventModel
                .getItemSku()))).thenReturn(null);
    Mockito.when(
        merchantProductMVIndexingFailedRepository.save(Mockito
            .any(MerchantProductMVIndexingFailed.class))).thenThrow(
        new RuntimeException("test failed to save to failed item skus"));
    service.update(eventModel);
    Mockito.verify(merchantProductMVRepository).findByStoreIdAndItemSku(
        Mockito.eq(eventModel.getStoreId()), Mockito.eq(eventModel.getItemSku()));
    Mockito.verify(productLevel3Repository)
        .findSummaryByGdnSku(Mockito.eq(eventModel.getItemSku()));
    Mockito.verify(merchantProductMVIndexingFailedRepository).findByItemSku(
        Mockito.eq(eventModel.getItemSku()));
    Mockito.verify(merchantProductMVIndexingFailedRepository).save(
        Mockito.any(MerchantProductMVIndexingFailed.class));
  }

  @Test
  public void update_ExistingItem_ValidTimestamp_EventTimestampIsGreater() {
    savedMv.setLastIndexedProductDate(DATE_YESTERDAY.getTime());
    eventModel.setTimestamp(now_milis);
    Mockito.when(
        merchantProductMVRepository.findByStoreIdAndItemSku(Mockito.anyString(),
            Mockito.anyString())).thenReturn(savedMv);
    service.update(eventModel);
    Mockito.verify(merchantProductMVRepository).save(savedMv);
  }

  @Test
  public void update_ExistingItem_ValidTimestamp_MVTimestampIsNull() {
    eventModel.setTimestamp(now_milis);
    Mockito.when(
        merchantProductMVRepository.findByStoreIdAndItemSku(Mockito.anyString(),
            Mockito.anyString())).thenReturn(savedMv);
    service.update(eventModel);
    Mockito.verify(merchantProductMVRepository).save(savedMv);
  }

  @Test
  public void update_ExistingItem_InvalidTimestamp_EventModelTimestampIsLesser() {
    savedMv.setLastIndexedProductDate(DATE_NOW.getTime());
    eventModel.setTimestamp(yesterday_milis);
    Mockito.when(
        merchantProductMVRepository.findByStoreIdAndItemSku(Mockito.anyString(),
            Mockito.anyString())).thenReturn(savedMv);
    service.update(eventModel);
  }

}
