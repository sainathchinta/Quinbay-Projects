package com.gdn.partners.pcu.master.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryInfoWebResponse extends CategoryWebResponse {
  private List<ShippingWebResponse> shippingResponses;
  private List<CategoryReferenceWebResponse> masterCategoryReference;
  private List<CategoryReferenceWebResponse> salesCategoryReference;
  private List<CategoryAttributeWebResponse> categoryAttributes;

  @Builder(builderMethodName = "CategoryInfoWebResponse")
  private CategoryInfoWebResponse(String id, String name, String categoryCode, Integer sequence,
      String shortDescription, byte[] description, byte[] defaultDescription, String state, boolean display,
      Integer logisticAdjustment, boolean warranty, boolean needIdentity, boolean activated, boolean viewable,
      CatalogDetailResponse catalog, String parentCategoryId, Integer internalActivationInterval, String nameEnglish,
      byte[] descriptionEnglish, long childCount, List<ShippingWebResponse> shippingResponses, int dangerousGoodsLevel,
      List<CategoryReferenceWebResponse> masterCategoryReference,
      List<CategoryReferenceWebResponse> salesCategoryReference,
      List<CategoryAttributeWebResponse> categoryAttributes, boolean wholesalePriceConfigEnabled) {
    super(id, name, categoryCode, sequence, shortDescription, description, defaultDescription, state, display,
        logisticAdjustment, warranty, needIdentity, activated, viewable, catalog, parentCategoryId,
        internalActivationInterval, nameEnglish, descriptionEnglish, childCount, dangerousGoodsLevel,
        wholesalePriceConfigEnabled);
    this.shippingResponses = shippingResponses;
    this.categoryAttributes = categoryAttributes;
    this.masterCategoryReference = masterCategoryReference;
    this.salesCategoryReference = salesCategoryReference;
  }
}
