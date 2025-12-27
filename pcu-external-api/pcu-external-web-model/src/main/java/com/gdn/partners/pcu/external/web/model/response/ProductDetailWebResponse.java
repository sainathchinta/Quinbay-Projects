package com.gdn.partners.pcu.external.web.model.response;

import java.util.List;
import java.util.Set;

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
public class ProductDetailWebResponse {

  private String id;
  private String productCode;
  private String name;
  private Double length;
  private Double width;
  private Double weight;
  private Double height;
  private Double shippingWeight;
  private byte[] description;
  private byte[] longDescription;
  private String brand;
  private String uniqueSellingPoint;
  private String uom;
  private boolean activated;
  private boolean viewable;
  private String productStory;
  private String specificationDetail;
  private String url;
  private List<ImageWebResponse> images;
  private Integer productType;
  private boolean promoSKU;
  private Set<ProductItemWebResponse> productItemResponses;
  private List<ProductAttributeWebResponse> productAttributeResponses;
  private List<ProductCategoryWebResponse> productCategoryResponses;
  private List<String> categories;

}
