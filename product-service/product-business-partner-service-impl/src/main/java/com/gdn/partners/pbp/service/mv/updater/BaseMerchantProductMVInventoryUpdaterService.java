package com.gdn.partners.pbp.service.mv.updater;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;

public abstract class BaseMerchantProductMVInventoryUpdaterService<E extends GdnBaseDomainEventModel>
    extends BaseMerchantProductMVUpdaterService<E> {

  @Override
  protected long getLastIndexedTimestamp(MerchantProductMV mv) {
    return 0;
  }
}
