package com.gdn.x.product.service.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Data;

@Data
@ConfigurationProperties(value = "configuration.kafka.topic")
@Component
public class KafkaTopicProperties {
  private String deleteTerminatedSellerProductEvent; // com.gdn.product.analytics.permanent.delete.product.xproduct
  private String deleteTerminatedSellerProductStatusEvent; // com.gdn.product.analytics.permanent.delete.product.result
  private boolean autoStartup;
  private String sizeChartUpdateEvent; // com.gdn.pcb.size.chart.update.event
  private String xProductAttributeMigrationEvent; // com.gdn.x.product.special.attribute.populate
  private String odooCreationEvent;
  private String videoCompressionEvent;// com.gdn.x.product.odoo.product.creation
  private String wmsMasterDataItemEvent; // wms.master.data.item.event
  private String compressedVideoUpdateEvent; //com.gdn.video.processor.compressed.video.update
  private String pwpPromoSkuEvent; // com.gdn.partners.product.pricing.pwp.promo.sku.event
  private String productDataAutoFixHistory; //com.gdn.pbp.product.data.auto.fix.history
}
