package com.gdn.aggregate.platform.module.product.listener.model.raw;

import java.io.Serial;
import java.io.Serializable;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@AllArgsConstructor
@NoArgsConstructor
@SuperBuilder
public class RawItemCombinedUpsertEventModel extends BaseData implements Serializable {
  @Serial
  private static final long serialVersionUID = 2408654077477497803L;
  private Item item;
  private boolean migration;
  private boolean directSave;
}
