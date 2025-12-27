package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@EqualsAndHashCode(callSuper = false)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class MasterProductEditDTO implements ProductEditContext{
  private ProfileResponse profileResponse;
  private CategoryDetailResponse categoryDetailResponse;
  private int maxProductDimensionLimit;
  private ProductCollection productCollection;
  private List<RestrictedKeywordsByField> restrictedKeywordsByFieldList = new ArrayList<>();
  private boolean contentChanged;
  private AutoApprovalType autoApprovalType;
  private List<CategoryResponse> categoryResponses = new ArrayList<>();
  private boolean postLive;
  private RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
    new RestrictedKeywordsByFieldAndActionType(new ArrayList<>());
  private int action = 1; // set default action to manual review
  private String categoryRestrictedKeywordId;
  private Set<String> vendorErrorFields = new HashSet<>();
  private List<ConfigurationStatusResponse> configurationStatusResponseList = new ArrayList<>();
  List<String> reviewTypeList = new ArrayList<>();
  private boolean saveInternalHistory;
  private boolean autoApprovalEligible;
  private String contentType;
  private boolean takenDownProduct;
  private List<ProductImageEditRequest> productImageEditRequestList = new ArrayList<>();
  private ApiErrorCode apiErrorCode;
  boolean publishImageQcForContentChange;
  boolean autoCategoryChange;
  private String oldCategoryName;
  private String newCategoryName;
  private String categoryCode;
  Set<String> modifiedFields = new HashSet<>();
  Boolean cncActiveAtL5;
  private boolean brandChanged;
}
