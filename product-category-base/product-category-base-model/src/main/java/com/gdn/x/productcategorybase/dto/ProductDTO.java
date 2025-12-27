package com.gdn.x.productcategorybase.dto;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties (ignoreUnknown = true)
public class ProductDTO extends GdnBaseEntity {
  private static final long serialVersionUID = -8705049070169037425L;

  private String productCode;
  private String name;
  private List<ProductCategory> productCategories = new ArrayList<>();
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private byte[] description;
  private String brand;
  private String uniqueSellingPoint;
  private String uom;
  private boolean activated;
  private boolean viewable;
  private String productStory;
  private String specificationDetail;
  private String url;
  private boolean forReview = false;
  private List<ProductAttribute> productAttributes = new ArrayList<>();
  private List<ProductImage> productImages = new ArrayList<>();
  private List<ProductItem> productItems = new ArrayList<>();
  private boolean promoSKU;
  private boolean reviewPending;
  private String createdMerchant;
  private boolean edited;
  private boolean revised;
  private boolean pickedForDeletion;
  private boolean markForDelete;
  private Set<String> deletedItems = new HashSet<>();
  private String distributionInfo;
}
