package com.gdn.aggregate.platform.module.product.listener.model.raw;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.util.CompleteItemData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SivaItemCombinedUpsertEventModel extends BaseData implements Serializable {
  @Serial
  private static final long serialVersionUID = 4453308699226819131L;
  private Item item;
  private SivaItem sivaItem;
  private SaveParam saveParam;
  private boolean directSave;
  private Set<String> eligibleDataSourcesForUpsert = new HashSet<>();
}
