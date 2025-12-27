package com.gdn.partners.pbp.service.mv;

import java.util.Calendar;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.entity.mv.MerchantProductMV;
import com.gdn.partners.pbp.repository.mv.MerchantProductMVRepository;
import com.gdn.partners.pbp.service.mv.updater.MerchantProductMVLevel2InventoryMinimumStockUpdaterServiceBean;
import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;

public class MerchantProductMVLevel2InventoryMinimumStockUpdaterServiceBeanTest {
  @Mock
  MerchantProductMVRepository merchantProductMVRepository;

  @InjectMocks
  MerchantProductMVLevel2InventoryMinimumStockUpdaterServiceBean service;
  MerchantProductMV savedMv;
  Level2InventoryMinimumStockAlertEvent eventModel;

  private long now_milis;
  private long yesterday_milis;

  private Calendar DATE_NOW;
  private Calendar DATE_YESTERDAY;


  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    eventModel = new Level2InventoryMinimumStockAlertEvent();
    savedMv = new MerchantProductMV();

    DATE_NOW = Calendar.getInstance();
    now_milis = DATE_NOW.getTimeInMillis();
    DATE_YESTERDAY = Calendar.getInstance();
    DATE_YESTERDAY.set(Calendar.DATE, Calendar.getInstance().get(Calendar.DATE) - 1);
    yesterday_milis = DATE_YESTERDAY.getTimeInMillis();
  }


  @Test
  public void update_NotFound() {
    Mockito.when(
        merchantProductMVRepository.findByStoreIdAndItemSku(Mockito.anyString(),
            Mockito.anyString())).thenReturn(null);
    service.update(eventModel);
  }

  @Test
  public void update_Found_ValidTimestamp_EventTimestampIsGreater() {
    savedMv.setLastIndexedInventoryDate(DATE_YESTERDAY.getTime());
    eventModel.setTimestamp(now_milis);
    Mockito.when(
        merchantProductMVRepository.findByStoreIdAndItemSku(Mockito.any(),
            Mockito.any())).thenReturn(savedMv);
    service.update(eventModel);
    Mockito.verify(merchantProductMVRepository).save(savedMv);
  }

  @Test
  public void update_Found_ValidTimestamp_MVTimestampIsNull() {
    eventModel.setTimestamp(now_milis);
    Mockito.when(
        merchantProductMVRepository.findByStoreIdAndItemSku(Mockito.any(),
            Mockito.any())).thenReturn(savedMv);
    service.update(eventModel);
    Mockito.verify(merchantProductMVRepository).save(savedMv);
  }

  @Test
  public void update_Found_InvalidTimestamp_EventModelTimestampIsLesser() {
    savedMv.setLastIndexedInventoryDate(DATE_NOW.getTime());
    eventModel.setTimestamp(yesterday_milis);
    Mockito.when(
        merchantProductMVRepository.findByStoreIdAndItemSku(Mockito.anyString(),
            Mockito.anyString())).thenReturn(savedMv);
    service.update(eventModel);
  }
}
