package com.gdn.x.productcategorybase.dto.response;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import com.gdn.x.productcategorybase.dto.Image;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemResponse extends BaseDTOResponse {

  private static final long serialVersionUID = -9122123781691578828L;

  private String generatedItemName;
  private Double itemLength;
  private Double itemWidth;
  private Double itemHeight;
  private Double itemWeight;
  private Double itemDeliveryWeight;
  private String upcCode;
  private String skuCode;
  private boolean activated = false;
  private boolean viewable = false;
  private byte[] hash;
  private List<Image> images;
  private List<ProductItemAttributeValueResponse> productItemAttributeValueResponses = new ArrayList<>();
  private Integer dangerousGoodsLevel;
  private boolean internalUpdate;
  private boolean contentChanged;
  private String sourceItemCode;
  private Boolean vatApplicable;
  private boolean newlyAddedItem;

  private DistributionItemInfoResponse distributionItemInfo;
  private List<DimensionsAndUomResponse> dimensionsAndUOM = new ArrayList<>();

  public ProductItemResponse(String generatedItemName, Double itemLength, Double itemWidth, Double itemHeight,
      Double itemWeight, Double itemDeliveryWeight, String upcCode, String skuCode, boolean activated,
      boolean viewable, byte[] hash, List<Image> images) {
    this.generatedItemName = generatedItemName;
    this.itemLength = itemLength;
    this.itemWidth = itemWidth;
    this.itemHeight = itemHeight;
    this.itemWeight = itemWeight;
    this.itemDeliveryWeight = itemDeliveryWeight;
    this.upcCode = upcCode;
    this.skuCode = skuCode;
    this.activated = activated;
    this.viewable = viewable;
    this.hash = hash;
    this.images = images;
  }

  public ProductItemResponse(String generatedItemName, Double itemLength, Double itemWidth, Double itemHeight,
      Double itemWeight, Double itemDeliveryWeight, String upcCode, String skuCode, boolean activated,
      boolean viewable, byte[] hash, List<Image> images, Integer dangerousGoodsLevel) {
    this(generatedItemName, itemLength, itemWidth, itemHeight, itemWeight, itemDeliveryWeight, upcCode, skuCode,
        activated, viewable, hash, images);
    this.dangerousGoodsLevel = dangerousGoodsLevel;
  }

  public ProductItemResponse(String generatedItemName, Double itemLength, Double itemWidth, Double itemHeight,
      Double itemWeight, Double itemDeliveryWeight, String upcCode, String skuCode, boolean activated,
      boolean viewable, byte[] hash, List<Image> images, Integer dangerousGoodsLevel, boolean internalUpdate) {
    this(generatedItemName, itemLength, itemWidth, itemHeight, itemWeight, itemDeliveryWeight, upcCode, skuCode,
        activated, viewable, hash, images);
    this.dangerousGoodsLevel = dangerousGoodsLevel;
    this.internalUpdate = internalUpdate;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!super.equals(obj)) {
      return false;
    }
    if (this.getClass() != obj.getClass()) {
      return false;
    }
    ProductItemResponse other = (ProductItemResponse) obj;
    if (!Arrays.equals(this.hash, other.hash)) {
      return false;
    }
    return true;
  }

  public List<Image> getImages() {
    if (this.images == null) {
      this.images = new ArrayList<Image>();
    }
    return this.images;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = (prime * result) + Arrays.hashCode(this.hash);
    return result;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductItemResponse [generatedItemName=%s, itemLength=%s, itemWidth=%s, itemHeight=%s, itemWeight=%s, itemDeliveryWeight=%s, upcCode=%s, skuCode=%s, activated=%s, viewable=%s, hash=%s, images=%s, dangerousGoodsLevel=%s, getDangerousGoodsLevel()=%s, getGeneratedItemName()=%s, getHash()=%s, getImages()=%s, getItemDeliveryWeight()=%s, getItemHeight()=%s, getItemLength()=%s, getItemWeight()=%s, getItemWidth()=%s, getSkuCode()=%s, getUpcCode()=%s, hashCode()=%s, isActivated()=%s, isViewable()=%s, internalUpdate()=%s, isContentChanged()=%s, getSourceItemCode()=%s]",
            generatedItemName, itemLength, itemWidth, itemHeight, itemWeight, itemDeliveryWeight, upcCode, skuCode,
            activated, viewable, Arrays.toString(hash), images, dangerousGoodsLevel, getDangerousGoodsLevel(),
            getGeneratedItemName(), Arrays.toString(getHash()), getImages(), getItemDeliveryWeight(), getItemHeight(),
            getItemLength(), getItemWeight(), getItemWidth(), getSkuCode(), getUpcCode(), hashCode(), isActivated(),
            isViewable(), isInternalUpdate(), isContentChanged(), getSourceItemCode());
  }

}
