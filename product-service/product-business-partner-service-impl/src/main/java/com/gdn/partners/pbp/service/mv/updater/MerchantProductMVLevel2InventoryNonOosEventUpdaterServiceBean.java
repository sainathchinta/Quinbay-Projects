package com.gdn.partners.pbp.service.mv.updater;

import java.util.Date;

import org.springframework.stereotype.Service;

import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;

@Service("merchantProductMVLevel2InventoryNonOosEventUpdaterService")
public class MerchantProductMVLevel2InventoryNonOosEventUpdaterServiceBean extends
    BaseMerchantProductMVInventoryUpdaterService<Level2InventoryNonOosEvent> {

  @Override
  protected MerchantProductMV getMaterializedView(Level2InventoryNonOosEvent eventModel) {
    return this.merchantProductMVRepository.findByStoreIdAndItemSku(eventModel.getStoreId(),
        eventModel.getLevel2Id());
  }

  @Override
  protected void updateMaterializedView(MerchantProductMV mv, Level2InventoryNonOosEvent eventModel) {
    mv.setOutOfStockStatus(false);
    mv.setLastIndexedInventoryDate(new Date());
    this.merchantProductMVRepository.save(mv);
  }
}
