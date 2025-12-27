package com.gdn.partners.pbp.service.mv.updater;

import org.springframework.beans.factory.annotation.Autowired;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;
import com.gdn.partners.pbp.repository.mv.MerchantProductMVRepository;

public abstract class BaseMerchantProductMVUpdaterService<E extends GdnBaseDomainEventModel>
    extends BaseMaterializedViewUpdaterService<MerchantProductMV, E> {

  @Autowired
  protected MerchantProductMVRepository merchantProductMVRepository;

  @Override
  protected void handleNotFoundMaterializedView(E eventModel) {
    // do nothing
  }
}
