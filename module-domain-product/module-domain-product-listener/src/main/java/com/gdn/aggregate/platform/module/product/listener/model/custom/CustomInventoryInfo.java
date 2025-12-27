package com.gdn.aggregate.platform.module.product.listener.model.custom;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
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
@Document(collection = Collections.INVENTORY_INFORMATION_COLLECTION)
public class CustomInventoryInfo extends BaseData {

  private String itemSku;

  private boolean cnc;

  private String type;

  private List<StockInformation> stockInformations;

  @Data
  @Builder
  @AllArgsConstructor
  @NoArgsConstructor
  public static class StockInformation {

    private String location;

    private int availableStock;

    private int originalStock;

    private String status;

    List<StockInformationDetail> stockInformationDetails;

  }

  @Data
  @Builder
  @AllArgsConstructor
  @NoArgsConstructor
  public static class StockInformationDetail {

    private Integer safetyStock;

    private int availableStock;

    private int originalStock;

    private String pickupPointCode;

    private String status;

  }

}