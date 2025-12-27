package com.gdn.partners.pbp.service.mv.updater;

import java.util.Date;

import org.springframework.stereotype.Service;

import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;

@Service("merchantProductMVLevel2InventoryMinimumStockUpdaterService")
public class MerchantProductMVLevel2InventoryMinimumStockUpdaterServiceBean extends
    BaseMerchantProductMVInventoryUpdaterService<Level2InventoryMinimumStockAlertEvent> {

  @Override
  protected MerchantProductMV getMaterializedView(Level2InventoryMinimumStockAlertEvent eventModel) {
    return this.merchantProductMVRepository.findByStoreIdAndItemSku(eventModel.getStoreId(),
        eventModel.getGdnSku());
  }

  @Override
  protected void updateMaterializedView(MerchantProductMV mv,
      Level2InventoryMinimumStockAlertEvent eventModel) {
    mv.setBelowMinimumStockStatus(true);
    mv.setLastIndexedInventoryDate(new Date());
    merchantProductMVRepository.save(mv);
  }

}
