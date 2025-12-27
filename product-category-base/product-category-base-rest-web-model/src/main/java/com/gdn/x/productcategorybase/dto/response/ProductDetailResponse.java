package com.gdn.x.productcategorybase.dto.response;

import java.util.List;
import java.util.Set;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductDetailResponse extends ProductResponse {

  private static final long serialVersionUID = 322779459590123185L;

  private Set<ProductItemResponse> productItemResponses;
  private List<ProductAttributeResponse> productAttributeResponses;
  private List<ProductCategoryResponse> productCategoryResponses;
  private List<String> categories;
  private List<String> categoriesEnglish;
  private List<String> categoryCodes;
  private VideoDTO videoDTO;
  private DistributionInfoResponse distributionInfoResponse;
  private AiGeneratedFieldsResponse aiGeneratedFieldsResponse;

  public ProductDetailResponse(ProductResponse product) {
    super(new Builder().productCode(product.getProductCode()).name(product.getName())
        .length(product.getLength()).width(product.getWidth()).weight(product.getWeight())
        .height(product.getHeight()).shippingWeight(product.getShippingWeight())
        .description(product.getDescription()).longDescription(product.getLongDescription())
        .brand(product.getBrand()).uniqueSellingPoint(product.getUniqueSellingPoint())
        .uom(product.getUom()).activated(product.isActivated()).viewable(product.isViewable())
        .productStory(product.getProductStory()).productType(product.getProductType())
        .url(product.getUrl()).images(product.getImages())
        .specificationDetail(product.getSpecificationDetail()));
  }

}