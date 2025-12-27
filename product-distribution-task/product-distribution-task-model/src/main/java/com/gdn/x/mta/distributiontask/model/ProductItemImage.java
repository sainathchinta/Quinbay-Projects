package com.gdn.x.mta.distributiontask.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Table(name = "PDT_PRODUCT_ITEM_IMAGE")
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductItemImage extends GdnBaseEntity {
  private static final long serialVersionUID = 1L;
  @Column(name = "LOCATION_PATH")
  private String locationPath;

  @ToString.Exclude
  @JoinColumn(name = "PRODUCT_ITEM")
  @ManyToOne
  @JsonIgnore
  private ProductItem productItem;

  @Column(name = "IS_MAIN_IMAGE")
  private Boolean mainImage;

  @Column(name = "SEQUENCE")
  private Integer sequence;

  @Column(name = "ORIGINAL_IMAGE", columnDefinition = "Boolean")
  private Boolean originalImage;

  @Column(name = "HASH_CODE")
  private String hashCode;

  @Column(name = "IS_ACTIVE", columnDefinition = "boolean default false")
  private boolean active;

  @Column(name = "IS_EDITED", columnDefinition = "boolean default false")
  private boolean edited;

  @Column(name = "IS_REVISED", columnDefinition = "boolean default false")
  private boolean revised;

  @Column(name = "COMMON_IMAGE", columnDefinition = "boolean default false")
  private boolean commonImage;

  public ProductItemImage(String locationPath, ProductItem productItem, Boolean isMainImage,
      Integer sequence) {
    this.locationPath = locationPath;
    this.productItem = productItem;
    this.mainImage = isMainImage;
    this.sequence = sequence;
  }

  public ProductItemImage(String locationPath, ProductItem productItem, Boolean mainImage, Integer sequence,
      Boolean originalImage) {
    this.locationPath = locationPath;
    this.productItem = productItem;
    this.mainImage = mainImage;
    this.sequence = sequence;
    this.originalImage = originalImage;
  }

}
