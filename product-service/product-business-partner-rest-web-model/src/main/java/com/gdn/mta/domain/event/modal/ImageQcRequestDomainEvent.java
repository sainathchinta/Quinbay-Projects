package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ImageQcHashCodeAndLocationPathRequest;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.product.entity.CategoryHierarchyDTO;
import com.gdn.mta.product.entity.KeywordRequestDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ImageQcRequestDomainEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -8355767380038891921L;
  private String productCode;
  private Double minPrice;
  private Double maxPrice;
  private List<ImageQcHashCodeAndLocationPathRequest> images;
  private String productName;
  private String brand;
  private String description;
  private String categoryCode;
  private List<CategoryHierarchyDTO> categoryHierarchy;
  private String usp;
  private List<KeywordRequestDTO> restrictedKeywords = new ArrayList<>();
  private String sellerCode;

  public ImageQcRequestDomainEvent(String productCode, Double minPrice, Double maxPrice,
      List<ImageQcHashCodeAndLocationPathRequest> images, String productName, String brand, String description,
      String categoryCode, List<CategoryHierarchyDTO> categoryHierarchy) {
    this.productCode = productCode;
    this.minPrice = minPrice;
    this.maxPrice = maxPrice;
    this.images = images;
    this.productName = productName;
    this.brand = brand;
    this.description = description;
    this.categoryCode = categoryCode;
    this.categoryHierarchy = categoryHierarchy;
  }

  public ImageQcRequestDomainEvent(String productCode, Double minPrice, Double maxPrice,
      List<ImageQcHashCodeAndLocationPathRequest> images, String productName, String brand, String description,
      String categoryCode, List<CategoryHierarchyDTO> categoryHierarchy, String usp,
      List<KeywordRequestDTO> restrictedKeywords) {
    this.productCode = productCode;
    this.minPrice = minPrice;
    this.maxPrice = maxPrice;
    this.images = images;
    this.productName = productName;
    this.brand = brand;
    this.description = description;
    this.categoryCode = categoryCode;
    this.categoryHierarchy = categoryHierarchy;
    this.usp = usp;
    this.restrictedKeywords = restrictedKeywords;
  }
}
