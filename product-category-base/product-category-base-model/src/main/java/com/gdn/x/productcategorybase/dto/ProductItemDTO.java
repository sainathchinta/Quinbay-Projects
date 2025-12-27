package com.gdn.x.productcategorybase.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = false)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemDTO extends GdnBaseEntity {

  private static final long serialVersionUID = -374958437898251112L;
  private Product product;
  private String productId;
  private String generatedItemName;
  private String upcCode;
  private String skuCode;
  private boolean activated;
  private boolean viewable;
  private boolean internalUpdate;
  private byte[] hash;
  private String sourceItemCode;
  private boolean contentChanged = false;
  private List<ProductItemAttributeValue>
    productItemAttributeValues = new ArrayList<ProductItemAttributeValue>();
  private List<ProductItemImageDTO> productItemImageDTOS = new ArrayList<>();
  private Integer dangerousGoodsLevel;
  private Boolean vatApplicable;
  private boolean newlyAddedItem;
  private boolean markForDelete;
}
