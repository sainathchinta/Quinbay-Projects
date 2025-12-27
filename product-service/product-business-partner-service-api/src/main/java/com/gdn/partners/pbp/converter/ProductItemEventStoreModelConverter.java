package com.gdn.partners.pbp.converter;

import com.gdn.partners.pbp.entity.eventstore.ProductItemEventStore;
import com.gdn.x.product.domain.event.model.ItemChange;

public interface ProductItemEventStoreModelConverter {
  ProductItemEventStore convertToProductItemEventStore(ItemChange eventModel, String eventName);

  ItemChange convertToItemChange(ProductItemEventStore eventModel);
}
