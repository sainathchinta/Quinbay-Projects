package com.gdn.partners.pbp.service.eventstore;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.partners.pbp.entity.eventstore.EventStore;
import com.gdn.partners.pbp.service.sysparam.SystemParameterService;

public abstract class BaseEventStoreKafkaMessageProcessor<T extends GdnBaseDomainEventModel>
    implements EventStoreKafkaMessageProcessor<T> {

  @Autowired
  @Qualifier("globalSystemParameterService")
  private SystemParameterService systemParameterService;

  public SystemParameterService getSystemParameterService() {
    return systemParameterService;
  }

  public void setSystemParameterService(SystemParameterService systemParameterService) {
    this.systemParameterService = systemParameterService;
  }

  private static final Logger LOG = LoggerFactory
      .getLogger(BaseEventStoreKafkaMessageProcessor.class);

  @Override
  public void process(T eventModel, String eventName) {
    try {
      if (allowedToRun()) {
        Boolean isIndexingRunning =
            Boolean.valueOf(systemParameterService
                .getParameter("sysparam.productlevel3.materialized-view-indexing-running"));
        if (isIndexingRunning) {
          EventStore savedEventStore = saveToEventStore(eventModel, eventName);
          handleIndexingRunning(eventModel, savedEventStore);
        }
      }
    } catch (Exception e) {
      LOG.error("Error when processing event. EventName: {}, EventModel: {}", eventName,
          eventModel, e);
    }
  }

  protected abstract EventStore saveToEventStore(T eventModel, String eventName) throws Exception;

  protected abstract void handleIndexingRunning(T eventModel, EventStore savedEventStore)
      throws Exception;
}
