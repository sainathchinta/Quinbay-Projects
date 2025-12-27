package com.gdn.partners.pbp.service.mv.updater;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.partners.pbp.model.vo.MaterializedView;

public abstract class BaseMaterializedViewUpdaterService<M extends MaterializedView, E extends GdnBaseDomainEventModel>
    implements MaterializedViewUpdaterService<M, E> {

  private static final Logger LOG = LoggerFactory
      .getLogger(BaseMaterializedViewUpdaterService.class);

  @Override
  public void update(E eventModel) {
    M mv = getMaterializedView(eventModel);
    if (mv != null) {
      long eventTimestamp = eventModel.getTimestamp();
      long lastIndexedTimestamp = getLastIndexedTimestamp(mv);
      if (eventTimestamp > lastIndexedTimestamp) {
        LOG.info("Updating materialized view. Materialized View: {}, EventModel: {}", mv,
            eventModel);
        updateMaterializedView(mv, eventModel);
      }
    } else {
      handleNotFoundMaterializedView(eventModel);
    }
  }

  /**
   * get saved materialized view data given an event model
   * 
   * @param eventModel
   * @return saved materialized view
   */
  protected abstract M getMaterializedView(E eventModel);

  /**
   * update existing materialized view
   * 
   * @param mv
   * @param eventModel event model
   */
  protected abstract void updateMaterializedView(M mv, E eventModel);

  /**
   * handle not-existing materialized view data given an event model
   * 
   * @param eventModel
   */
  protected abstract void handleNotFoundMaterializedView(E eventModel);

  /**
   * get materialized view data last indexed time (used for handling old or duplicate event)
   * 
   * @param mv
   * @return
   */
  protected abstract long getLastIndexedTimestamp(M mv);

}
