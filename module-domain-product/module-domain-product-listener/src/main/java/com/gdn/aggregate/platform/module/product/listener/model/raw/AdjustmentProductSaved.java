package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.fasterxml.jackson.annotation.JsonProperty;
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
public class AdjustmentProductSaved extends BaseData {

  private String adjustmentId;

  @JsonProperty("productSku")
  private String itemSku;

  private String campaignCode;

  private String adjustmentName;

  private String description;

  private long value;

  private boolean activated;

  private long startDate;

  private long endDate;

  private int quota;

  private String budgetOwner;

  private String storeId;

  private String pickupPointCode;

  private Integer priority;

  @Override
  public List<String> toIds() {
    String id = MainUtil.toNotNullString(this.adjustmentId);
    return MainUtil.toList(id);
  }

}
