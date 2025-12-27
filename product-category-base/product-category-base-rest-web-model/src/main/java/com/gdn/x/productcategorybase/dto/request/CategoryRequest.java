package com.gdn.x.productcategorybase.dto.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryRequest extends BaseDTORequest {

  private static final long serialVersionUID = 5086001597043317432L;

  private CatalogRequest catalog;
  private CategoryRequest parentCategory;
  private String name;
  private String categoryCode;
  private Integer sequence;
  private String shortDescription;
  private byte[] description;
  private byte[] defaultDescription;
  private String state;
  private boolean display;
  private Integer logisticAdjustment;
  private boolean warranty = false;
  private boolean needIdentity;
  private boolean activated = false;
  private boolean viewable = false;
  private Integer internalActivationInterval;
  private boolean umkm;
  private List<CategoryAttributeRequest> categoryAttributes = new ArrayList<CategoryAttributeRequest>();
  private List<ProductCategoryRequest> productCategories = new ArrayList<ProductCategoryRequest>();
  private List<CategoryReferenceRequest> masterCategoryReferences = new ArrayList<CategoryReferenceRequest>();
  private List<CategoryReferenceRequest> salesCategoryReferences = new ArrayList<CategoryReferenceRequest>();

  public CategoryRequest() {}

  public CategoryRequest(CatalogRequest catalog, CategoryRequest parentCategory, String name, String categoryCode,
      Integer sequence, String shortDescription, byte[] description, byte[] defaultDescription, String state,
      boolean display, Integer logisticAdjustment, boolean warranty, boolean needIdentity, boolean activated,
      boolean viewable, List<CategoryAttributeRequest> categoryAttributes,
      List<ProductCategoryRequest> productCategories, String storeId) {
    this.catalog = catalog;
    this.parentCategory = parentCategory;
    this.name = name;
    this.categoryCode = categoryCode;
    this.sequence = sequence;
    this.shortDescription = shortDescription;
    this.description = description;
    this.defaultDescription = defaultDescription;
    this.state = state;
    this.display = display;
    this.logisticAdjustment = logisticAdjustment;
    this.warranty = warranty;
    this.needIdentity = needIdentity;
    this.activated = activated;
    this.viewable = viewable;
    this.categoryAttributes = categoryAttributes;
    this.productCategories = productCategories;
    this.setStoreId(storeId);
  }

  @Override
  public String toString() {
    return String.format(
        "Category [catalog=%s, parentCategory=%s, name=%s, categoryCode=%s, sequence=%s, "
            + "shortDescription=%s, description=%s, viewable=%s, internalActivationInterval=%s, umkm=%s"
            + "toString()=%s]",
        this.catalog, this.parentCategory, this.name, this.categoryCode, this.sequence,
        this.shortDescription, this.description == null ? "" : new String(this.description),
        this.viewable, this.internalActivationInterval, this.umkm, super.toString());
  }
}
