package com.gdn.partners.pcu.external.web.model.request;

import java.util.ArrayList;
import java.util.List;

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
public class ProductAttributeWebRequest {

  private String id;
  private AttributeWebRequest attribute;
  private String productAttributeName;
  private boolean isOwnByProductItem;
  private Integer sequence;
  private List<ProductAttributeValueWebRequest> productAttributeValues = new ArrayList<>();
}
