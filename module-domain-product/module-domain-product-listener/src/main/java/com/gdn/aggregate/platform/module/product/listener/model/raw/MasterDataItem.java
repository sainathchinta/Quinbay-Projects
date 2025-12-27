package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.ArrayList;
import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.MASTER_DATA_ITEM)
public class MasterDataItem extends BaseData {

  private String productCode;

  private String skuCode;

  private String generatedItemName;

  private String upcCode;

  private boolean activated;

  private boolean viewable;

  private String hash;

  private String inventoryType;

  private int dangerousLevel;

  private Boolean newData;

  private List<MasterDataItemAttributeValue> masterDataItemAttributeValues = new ArrayList<MasterDataItemAttributeValue>();

  private List<MasterDataItemImage> masterDataItemImages = new ArrayList<MasterDataItemImage>();

  private Double itemDeliveryWeight;

  private Double itemHeight;

  private Double itemLength;

  private Double itemWeight;

  private Double itemWidth;

  private boolean reviewPending;

  @Override
  public List<String> toIds() {
    String id = MainUtil.toNotNullString(this.skuCode);
    return MainUtil.toList(id);
  }

  @Data
  @Builder
  @NoArgsConstructor
  @AllArgsConstructor
  public static class MasterDataItemAttributeValue {

    private String attributeValue;

    private MasterDataAttribute masterDataAttribute;

  }

  @Data
  @Builder
  @NoArgsConstructor
  @AllArgsConstructor
  public static class MasterDataAttribute {

    private String attributeCode;

    private String attributeType;

    private boolean mandatory;

    private String attributeName;

    private boolean searchable;

    private String description;

    private String example;

    private boolean skuValue;

  }

  @Data
  @Builder
  @NoArgsConstructor
  @AllArgsConstructor
  public static class MasterDataItemImage {

    private boolean mainImage;

    private String locationPath;

    private int sequence;

  }

}
