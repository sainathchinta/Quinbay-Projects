package com.gdn.aggregate.platform.module.product.listener.model.semi;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.SIVA_FLASHSALE_SCHEDULE)
public class SivaFlashsaleSchedule extends BaseData {

  private long start;

  private long end;

  private String logo;

  private String subFlashsaleUrl;

  private Background background;

  private boolean timeBased;

  @Data
  @Builder
  @NoArgsConstructor
  @AllArgsConstructor
  public static class Background {

    private String start;

    private String end;

    private String imageUrl;

  }

}
