package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class FlashsaleGroup extends BaseData {

  private String groupId;

  private String title;

  private String logo;

  private Integer sequence;

  @Override
  public List<String> toIds() {
    String id = MainUtil.toNotNullString(this.groupId);
    return MainUtil.toList(id);
  }

}
