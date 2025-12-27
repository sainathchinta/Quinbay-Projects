package com.gdn.partners.pcu.external.web.model.response;

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
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3WebResponse {
  private static final long serialVersionUID = 5396987035880931557L;
  private String id;
  private String productSku;
  private String productCode;
  private String businessPartnerCode;
  private Boolean synchronize;
  private String productName;
  private Integer productType;
  private String categoryCode;
  private String categoryName;
  private String categoryHierarchy;
  private String brand;
  private String description;
  private String specificationDetail;
  private String uniqueSellingPoint;
  private String productStory;
  private List<ProductItemLevel3WebResponse> items;
  private List<ProductLevel3AttributeWebResponse> attributes;
  private List<ProductLevel3ImageWebResponse> images;
  private String url;
  private Boolean installationRequired;
  private String categoryId;
  private boolean enableEdit = true;
  private Long version;
  private boolean wholesalePriceConfigEnabled;
  private ProductScoreResponse productScore;
  private boolean isSuspended;
  private boolean productEditable;
  private List<DistinctPickUpPoint> distinctPickUpPoints;
}
