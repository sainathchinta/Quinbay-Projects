package com.gdn.aggregate.platform.module.product.listener.model.partial;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class PartialCleanSivaFlashsaleGroup extends BaseData {

  private List<SivaFlashsaleSchedule> schedules;

}
