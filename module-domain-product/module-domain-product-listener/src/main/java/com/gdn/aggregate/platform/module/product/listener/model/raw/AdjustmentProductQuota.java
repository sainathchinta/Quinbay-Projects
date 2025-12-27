package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
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
@Document(collection = Collections.ADJUSTMENT_PRODUCT_QUOTA)
public class AdjustmentProductQuota extends BaseData {

  private String adjustmentProductId;

  private String itemSku;

  private String campaignCode;

  private String adjustmentName;

  private int quota;

  private int usedQuota;

  private int usedQuotaPerTransaction;

  private String budgetOwner;

  private String storeId;

  private String pickupPointCode;

  private Integer priority;

  @Override
  public List<String> toIds() {
    String id = MainUtil.toNotNullString(this.adjustmentProductId);
    return MainUtil.toList(id);
  }

}
