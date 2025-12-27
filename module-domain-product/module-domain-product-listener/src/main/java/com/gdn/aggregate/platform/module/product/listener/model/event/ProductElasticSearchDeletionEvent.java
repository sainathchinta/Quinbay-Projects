package com.gdn.aggregate.platform.module.product.listener.model.event;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.Set;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductElasticSearchDeletionEvent extends BaseData implements Serializable {
  @Serial
  private static final long serialVersionUID = 8531104612158783472L;
  private Set<String> productSkus;
  private String traceId;
  private long timestamp;
}