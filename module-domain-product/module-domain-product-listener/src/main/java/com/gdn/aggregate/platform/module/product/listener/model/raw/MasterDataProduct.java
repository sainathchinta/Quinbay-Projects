package com.gdn.aggregate.platform.module.product.listener.model.raw;

import java.util.List;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
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
@Document(collection = Collections.MASTER_DATA_PRODUCT)
public class MasterDataProduct extends BaseData {

  private String productCode;

  private String brand;

  private String brandLogoUrl;

  private double shippingWeight;

  private String specificationDetail;

  private String productName;

  private String description;

  private String longDescription;

  private String uniqueSellingPoint;

  private boolean activated;

  private boolean viewable;

  private String productStory;

  private String uom;

  private Boolean newData;

  private List<MasterDataProductImage> masterDataProductImages;

  private List<MasterDataProductAttribute> masterDataProductAttributes;

  private String url;

  private Double length;

  private Double width;

  private Double height;

  private Double weight;

  @Override
  public List<String> toIds() {
    String id = MainUtil.toNotNullString(productCode);
    return MainUtil.toList(id);
  }

  @Data
  @Builder
  @NoArgsConstructor
  @AllArgsConstructor
  public static class MasterDataProductImage {

    private boolean mainImage;

    private String locationPath;

    private String productCode;

    private int sequence;

  }

  @Data
  @Builder
  @NoArgsConstructor
  @AllArgsConstructor
  public static class MasterDataProductAttribute {

    private boolean ownByProductItem;

    private MasterDataAttribute masterDataAttribute;

    private int sequence;

    private List<MasterDataProductAttributeValue> masterDataProductAttributeValues;

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
  public static class MasterDataProductAttributeValue {

    private String descriptiveAttributeValue;

    private String descriptiveAttributeValueType;

    private String allowedAttributeValueCode;

    private String predefinedAllowedAttributeValueCode;

    private MasterDataAllowedAttributeValue allowedAttributeValue;

  }

  @Data
  @Builder
  @NoArgsConstructor
  @AllArgsConstructor
  public static class MasterDataAllowedAttributeValue {

    private String allowedAttributeValueCode;

    private String value;

    private Integer sequence;

  }

}
