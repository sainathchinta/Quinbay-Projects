package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class CampaignProductPublished extends BaseData {

  private String campaignCode;

  private String campaignName;

  private long promotionStartTime;

  private long promotionEndTime;

  private String tagLabel;

  private boolean exclusive;

  private boolean retainData;

  private String campaignType;

  private String flashSaleCampaignName;

  private int priority;

  private String backgroundImageUrl;

  private List<ProductSkuEventModel> skuList;

  private boolean emptyQuota;

  @Override
  public List<String> toIds() {
    String id = MainUtil.toNotNullString(this.campaignCode);
    return MainUtil.toList(id);
  }

  @Data
  @Builder
  @AllArgsConstructor
  @NoArgsConstructor
  public static class ProductSkuEventModel {

    private String productSku;

    private String itemSku;

    private String pickupPointCode;

    private String itemPickupPointId;

    private Double discount;

    private int quota;

    private Integer sessionId;

    private Double blibliDiscount;

    private Integer blibliQuota;

    private String campaignPrice;

    private double finalPrice;

    private List<String> groupIds;

    private boolean timeBased;

    private boolean processZeroDiscount;

    private boolean clearance;

    private boolean markForDelete;

  }

}
