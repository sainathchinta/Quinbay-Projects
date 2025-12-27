package com.gdn.partners.pbp.service.mv.updater;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.partners.pbp.model.vo.MaterializedView;

public interface MaterializedViewUpdaterService<M extends MaterializedView, E extends GdnBaseDomainEventModel> {

  void update(E eventModel);
}
