package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;
import java.util.Map;

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
public class ProductItemWebRequest {

  private String id;
  private String generatedItemName;
  private String upcCode;
  private String skuCode;
  private boolean activated;
  private boolean viewable;
  private byte[] hash;
  List<ProductItemAttributeValueWebRequest> productItemAttributeValues;
  private List<ImageRequest> images;
  private Integer dangerousGoodsLevel;
  private Map<String, String> attributesMap;
}
