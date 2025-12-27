package com.gdn.partners.pcu.internal.web.model.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 20/12/2018 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class ProductAttributeWebRequest extends BaseWebRequest {

  private String id;
  private AttributeWebRequest attribute;
  private String productAttributeName;
  private boolean isOwnByProductItem;
  private Integer sequence;
  private List<ProductAttributeValueWebRequest> productAttributeValues = new ArrayList<>();
}