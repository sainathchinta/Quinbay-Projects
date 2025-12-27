package com.gdn.partners.pbp.service.mv.updater;

import java.util.Date;

import org.springframework.stereotype.Service;

import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;


@Service("merchantProductMVLevel2InventoryOosEventUpdaterService")
public class MerchantProductMVLevel2InventoryOosEventUpdaterServiceBean extends
    BaseMerchantProductMVInventoryUpdaterService<Level2InventoryOosEvent> {

  @Override
  protected MerchantProductMV getMaterializedView(Level2InventoryOosEvent eventModel) {
    return this.merchantProductMVRepository.findByStoreIdAndItemSku(eventModel.getStoreId(),
        eventModel.getLevel2Id());
  }

  @Override
  protected void updateMaterializedView(MerchantProductMV mv, Level2InventoryOosEvent eventModel) {
    mv.setOutOfStockStatus(true);
    mv.setLastIndexedInventoryDate(new Date());
    this.merchantProductMVRepository.save(mv);
  }
}
