package com.gdn.x.product.dao.api;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.model.entity.OfflineItem;

import java.util.List;
import java.util.Set;

public interface OfflineItemRepositoryCustom {

  void upsertOfflineItem(MandatoryRequestParam mandatoryRequestParam, OfflineItem offlineItem,
      boolean markForDelete);

  void updatePriceByItemSku(MandatoryRequestParam mandatoryRequestParam, String merchantCode, OfflineItem offlineItem);

  List<OfflineItem> findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndLimit(String storeId,
      Set<String> itemSkus, int limit);
}
