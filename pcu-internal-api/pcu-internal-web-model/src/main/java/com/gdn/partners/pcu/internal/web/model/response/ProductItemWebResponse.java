package com.gdn.partners.pcu.internal.web.model.response;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemWebResponse {

  private String id;
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
  private List<ImageWebResponse> images;
  private List<ProductItemAttributeValueWebResponse> productItemAttributeValueResponses;
  private Integer dangerousGoodsLevel;
  private String thumbnailPath;
  private boolean thumbnailActive;
  private Boolean vatApplicable;
  private boolean newlyAdded;
  private Double minPrice;
  private Double maxPrice;
  private String itemSku;
  private String pdpRedirectionLink;
}
