package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.FLASHSALE_PRODUCT)
public class FlashsaleProduct extends BaseData {

  private String productSku;

  private String itemSku;

  private String pickupPointCode;

  private String productType;

  private String campaignCode;

  private Integer sessionId;

  private int sequence;

  private SivaFlashsaleSchedule schedule;

  private List<FlashsaleQuota> flashsaleQuotas;

  private boolean active;

  private boolean exclusive;

  private List<String> groupIds;

  @Override
  public List<String> toIds() {
    return ModuleProductUtil.toFlashsaleProductIds(this.productSku,this.itemSku,this.campaignCode,ModuleProductUtil.isFlashsaleProductTimeBased(this));
  }

  @Data
  @Builder
  @NoArgsConstructor
  @AllArgsConstructor
  public static class FlashsaleQuota {

    private String id;

    private String owner;

    private Integer quota;

  }

}
