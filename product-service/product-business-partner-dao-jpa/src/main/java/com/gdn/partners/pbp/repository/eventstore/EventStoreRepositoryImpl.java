package com.gdn.partners.pbp.repository.eventstore;

import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;

import com.gdn.partners.pbp.entity.eventstore.EventStore;
import com.gdn.partners.pbp.entity.eventstore.Level2InventoryEventStore;
import com.gdn.partners.pbp.entity.eventstore.ProductItemEventStore;

public class EventStoreRepositoryImpl implements EventStoreRepositoryCustom {
  @PersistenceContext
  private EntityManager em;

  @SuppressWarnings("unchecked")
  @Override
  public List<EventStore> findByLastUpdateEventAndGroupByItemSku() {
    List<EventStore> result = new ArrayList<>();
    Query productItemEventStoreQuery =
        em.createNativeQuery(
            "SELECT es.id, pies.item_sku, es.event_timestamp FROM event_store es JOIN product_item_event_store pies ON es.id = pies.id JOIN (SELECT MAX(event_timestamp) as last_event_timestamp, t_pi.item_sku FROM event_store t_e JOIN product_item_event_store t_pi ON t_e.id = t_pi.id GROUP BY t_pi.item_sku) AS res ON pies.item_sku = res.item_sku AND es.event_timestamp = res.last_event_timestamp ORDER BY es.stored_date ASC",
            ProductItemEventStore.class);
    Query level2InventoryEventStoreQuery =
        em.createNativeQuery(
            "SELECT es.id, ies.level2_id, es.event_timestamp, ies.inventory_status FROM event_store es JOIN level2_inventory_event_store ies ON es.id = ies.id JOIN (SELECT MAX(event_timestamp) as last_event_timestamp, t_ie.level2_id FROM event_store t_e  JOIN level2_inventory_event_store t_ie ON t_e.id = t_ie.id GROUP BY t_ie.level2_id) res  ON ies.level2_id = res.level2_id AND es.event_timestamp = res.last_event_timestamp ORDER BY es.stored_date ASC",
            Level2InventoryEventStore.class);
    List<ProductItemEventStore> productItemResult = productItemEventStoreQuery.getResultList();
    List<Level2InventoryEventStore> level2InventoryResult =
        level2InventoryEventStoreQuery.getResultList();
    result.addAll(productItemResult);
    result.addAll(level2InventoryResult);
    return result;
  }
}
