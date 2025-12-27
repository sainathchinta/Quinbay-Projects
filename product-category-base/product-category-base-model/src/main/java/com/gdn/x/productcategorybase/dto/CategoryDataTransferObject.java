package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.productcategorybase.ExtractionType;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryReference;
import com.gdn.x.productcategorybase.entity.CategoryRestrictedKeyword;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryDataTransferObject extends BaseResponse {

  private Catalog catalog;

  private Category parentCategory;

  private String parentCategoryId;

  private String name;

  private String categoryCode;

  private Integer sequence;

  private String shortDescription;

  private byte[] description;

  private byte[] defaultDescription;

  private String state;

  private boolean display;

  private Integer logisticAdjustment;

  private boolean warranty;

  private boolean needIdentity;

  private boolean activated = false;

  private boolean viewable = false;

  private Integer internalActivationInterval;

  private String nameEnglish;

  private byte[] descriptionEnglish;

  private int dangerousGoodsLevel = 0;

  private boolean umkm;

  private boolean genericTemplateEligible = false;

  private boolean wholesalePriceConfigEnabled;

  private String documentType;

  private List<CategoryAttribute> categoryAttributes = new ArrayList<CategoryAttribute>();

  private List<ProductCategory> productCategories = new ArrayList<ProductCategory>();

  private List<CategoryReference> masterCategoryReferences = new ArrayList<CategoryReference>();

  private List<CategoryReference> salesCategoryReferences = new ArrayList<CategoryReference>();

  private List<CategoryRestrictedKeyword> categoryRestrictedKeywords = new ArrayList<>();

  private OriginalSalesCategory originalSalesCategory;

  private Date oscUpdatedDate;

  private String oscUpdatedBy;

  private ExtractionType extractionType;

  private boolean b2bExclusive;
}
