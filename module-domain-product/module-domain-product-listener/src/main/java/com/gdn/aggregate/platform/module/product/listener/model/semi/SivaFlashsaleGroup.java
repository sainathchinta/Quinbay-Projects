package com.gdn.aggregate.platform.module.product.listener.model.semi;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.SIVA_FLASHSALE_GROUP)
public class SivaFlashsaleGroup extends BaseData {

  private String groupId;

  private String title;

  private String logo;

  private Integer sequence;

  private List<SivaFlashsaleSchedule> schedules;

  @Override
  public List<String> toIds() {
    String id = MainUtil.toNotNullString(this.groupId);
    return MainUtil.toList(id);
  }

}
