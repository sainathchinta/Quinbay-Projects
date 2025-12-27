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
@Table(name = "PDT_PRODUCT_IMAGE")
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductImage extends GdnBaseEntity {
  private static final long serialVersionUID = 1L;

  // brand_name_productcode_full01.jpg
  @Column(name = "LOCATION_PATH")
  private String locationPath;

  @Column(name = "SEQUENCE")
  private Integer sequence;

  @Column(name = "IS_MAIN_IMAGES", columnDefinition = "boolean default false")
  private boolean mainImage;

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

  @ToString.Exclude
  @JoinColumn(name = "PRODUCT")
  @ManyToOne
  @JsonIgnore
  private Product product;

  public ProductImage(Product product, String locationPath, Integer sequence, boolean mainImage) {
    this.product = product;
    this.locationPath = locationPath;
    this.sequence = sequence;
    this.mainImage = mainImage;
  }

  public ProductImage(String locationPath, Integer sequence, boolean mainImage, Boolean originalImage,
      Product product) {
    this.locationPath = locationPath;
    this.sequence = sequence;
    this.mainImage = mainImage;
    this.originalImage = originalImage;
    this.product = product;
  }

}
