package com.gdn.partners.pbp.service.mv.updater;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;

public abstract class BaseMerchantProductMVProductItemUpdaterService<E extends GdnBaseDomainEventModel>
    extends BaseMerchantProductMVUpdaterService<E> {

  /**
   * always update since no event store mechanism, so system would not know last update event time
   */
  @Override
  protected long getLastIndexedTimestamp(MerchantProductMV mv) {
    return 0;
  }
}
