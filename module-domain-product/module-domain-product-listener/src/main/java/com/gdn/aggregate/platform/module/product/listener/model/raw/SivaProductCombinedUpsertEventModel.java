package com.gdn.aggregate.platform.module.product.listener.model.raw;


import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.io.Serial;
import java.io.Serializable;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class SivaProductCombinedUpsertEventModel extends BaseData implements Serializable {
  @Serial
  private static final long serialVersionUID = 857179650570724789L;
  private SivaProduct sivaProduct;
  private SaveParam saveParam;
  private String eventTrigger;
}
