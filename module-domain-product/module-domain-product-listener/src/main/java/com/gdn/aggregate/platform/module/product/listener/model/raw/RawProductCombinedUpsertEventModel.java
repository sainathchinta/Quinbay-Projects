package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RawProductCombinedUpsertEventModel extends BaseData implements Serializable {
  @Serial
  private static final long serialVersionUID = 2115696651938788643L;
  private Product product;
  private SaveParam saveParam;
  private String eventTrigger;
  private MasterData masterData;
}
