package com.gdn.aggregate.platform.module.product.listener.model.semi;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;
import java.util.Map;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.SIVA_CAMPAIGN_PRODUCT)
public class SivaCampaignProduct extends BaseData {

  private Integer priority;

  private Integer activateSession;

  private boolean timeBased;

  private long promotionStartTime;

  private long promotionEndTime;

  private boolean active;

  private String campaignCode;

  private String campaignName;

  private String tagLabel;

  private boolean exclusive;

  private String productCardBackgroundColorHexcode;

  private Map<String, String> promoTabImageUrl;

  private Map<String, String> productCardTitleImageUrl;

  private Map<String, String> productCardBackgroundImageUrl;

  private Map<String, String> productCardBadgeImageUrl;

  @Override
  public List<String> toIds() {
    return ModuleProductUtil.toSivaCampaignProductIds(this);
  }

}
