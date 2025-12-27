package com.gdn.x.product.rest.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Configuration
public class SwitchContext {

  @Value("${pcb.master.data.enabled}")
  boolean usePcbMasterData;

  @Value("${main.image.from.main.image.url.for.unsync}")
  boolean mainImageFromMainImageUrlForUnsync;

  @Value("${override.late.fulfillment.by.product.type}")
  boolean overrideLateFulfillmentByProductType;

  @Value("${subscription.at.l5.flow}")
  boolean subscriptionAtL5Flow;

  @Value("${cnc.warehouse.feature.switch}")
  boolean cncForWarehouseFeatureSwitch;

  @Value("${fetch.view.config.by.channel}")
  String fetchViewConfigByChannel;

  @Value("${remove.taken.down.l5.from.response}")
  boolean removeTakenDownL5FromResponse;
}
