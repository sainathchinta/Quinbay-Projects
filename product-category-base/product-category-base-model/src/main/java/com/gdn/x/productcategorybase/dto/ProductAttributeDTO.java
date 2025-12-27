package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = false)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeDTO extends GdnBaseEntity {

  private static final long serialVersionUID = 1245597248440447306L;

  private Attribute attribute;

  private String attributeId;

  private Product product;

  private String productId;

  private String productAttributeName;

  private boolean isOwnByProductItem;

  private Integer sequence;

  private boolean extractedValue;

  private List<ProductAttributeValue> productAttributeValues = new ArrayList<>();

  private boolean markForDelete;
}
