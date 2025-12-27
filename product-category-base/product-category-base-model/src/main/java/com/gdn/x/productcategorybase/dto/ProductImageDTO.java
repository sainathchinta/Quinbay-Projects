package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.entity.Product;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductImageDTO {

  private String productId;
  private String productCode;
  private String locationPath;
  private boolean isMainImage;
  private Integer sequence;
  private boolean active;
  private Product product;
  private boolean revised;
  private boolean edited;
  private Boolean originalImage;
  private boolean commonImage;
  private String hashCode;
  public ProductImageDTO(String productId, String productCode, String locationPath,
    boolean isMainImage, Integer sequence, boolean active) {
    this.productId = productId;
    this.productCode = productCode;
    this.locationPath = locationPath;
    this.isMainImage = isMainImage;
    this.sequence = sequence;
    this.active = active;
  }

}
