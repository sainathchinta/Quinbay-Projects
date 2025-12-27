package com.gdn.aggregate.platform.module.product.listener.model.custom;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
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
@Document(collection = Collections.INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION)
public class CustomInventoryPickupPointInfo extends BaseData {

  private String productCode;

  private String productSku;

  @JsonProperty("level2Id")
  private String itemSku;

  private String pickupPointCode;

  private String pickupPointLocation;

  @JsonProperty("level2MerchantCode")
  private String merchantCode;

  private String storeId;

  private boolean cnc;

  private Boolean syncStock;

  private int availableStock;

  private int originalStock;

  private Integer safetyStock;

  private List<WarehouseInfo> warehouseInfos;

  @Override
  public List<String> toIds() {
    String itmSku = MainUtil.toNotNullString(this.itemSku);
    String ppCode = MainUtil.toNotNullString(this.pickupPointCode);
    return MainUtil.toList(itmSku,ppCode);
  }

  @Data
  @Builder
  @NoArgsConstructor
  @AllArgsConstructor
  public static class WarehouseInfo {

    private String warehouseCode;

    private String warehouseName;

    private String warehouseLocation;

    private Integer availableStock;

    private Integer originalStock;

  }

}