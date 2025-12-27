package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.VideoAddEditRequest;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.entity.Product;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class EditProductResponse extends BaseResponse {

  private static final long serialVersionUID = 1775745307210122110L;
  private boolean productReview;
  private String reviewType;
  private ApiErrorCode apiErrorCode;
  private boolean toTakeDown;
  private List<VariantsErrorListResponse> variantsErrorList = new ArrayList<>();
  private boolean publishImageQcForContentChange = false;
  private int action = 1;
  private String categoryRestrictedKeywordId;
  private Set<String> vendorErrorFields = new HashSet<>();
  private ProfileResponse profileResponse;
  List<CategoryResponse> categoryResponses = new ArrayList<>();
  private List<NewlySavedItemResponse> newlySavedItemResponseList = new ArrayList<>();
  private Map<String, String> attributeCodeAndName = new HashMap<>();
  private Map<String, String> itemCodeToItemNameMapping = new HashMap<>();
  private List<ProductLevel3Logistics> productLevel3LogisticsRequest = new ArrayList<>();
  private RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType;
  private ProductDetailEditDTO productDetailEditDTO;
  private boolean newImageAdded;
  private List<ProductBundleRecipeRequest> productBundleRecipe;
  private Map<String, String> extraDeletedItems = new HashMap<>();
  private Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
  private Product product;
  private ProductCollection productCollection;
  private Boolean scheduleRemovedForStatusUpdate;
  private boolean postLive;
  private VideoAddEditRequest videoAddEditRequest;
  private Boolean videoUpdated;
  private boolean distributionInfoUpdated;
  private boolean youTubeUrlUpdated;
  private Map<String, String> distributionInfoRequest;
  private List<ProductItemDistributionInfoRequest> distributionAndUOMRequest = new ArrayList<>();
  private List<AuditTrailDto> auditTrailDtoList;

  public EditProductResponse(boolean productReview, String reviewType, ApiErrorCode apiErrorCode, boolean toTakeDown,
      List<VariantsErrorListResponse> variantsErrorList) {
    this.productReview = productReview;
    this.reviewType = reviewType;
    this.apiErrorCode = apiErrorCode;
    this.toTakeDown = toTakeDown;
    this.variantsErrorList = variantsErrorList;
  }

  public EditProductResponse(boolean productReview, String reviewType, ApiErrorCode apiErrorCode, boolean toTakeDown,
      List<VariantsErrorListResponse> variantsErrorList, boolean publishImageQcForContentChange) {
    this.productReview = productReview;
    this.reviewType = reviewType;
    this.apiErrorCode = apiErrorCode;
    this.toTakeDown = toTakeDown;
    this.variantsErrorList = variantsErrorList;
    this.publishImageQcForContentChange = publishImageQcForContentChange;
  }

  public EditProductResponse(boolean productReview, String reviewType, ApiErrorCode apiErrorCode, boolean toTakeDown,
      List<VariantsErrorListResponse> variantsErrorList, boolean publishImageQcForContentChange, int action,
      String categoryRestrictedKeywordId, Set<String> vendorErrorFields, ProfileResponse profileResponse,
      List<CategoryResponse> categoryResponses) {
    this.productReview = productReview;
    this.reviewType = reviewType;
    this.apiErrorCode = apiErrorCode;
    this.toTakeDown = toTakeDown;
    this.variantsErrorList = variantsErrorList;
    this.publishImageQcForContentChange = publishImageQcForContentChange;
    this.action = action;
    this.categoryRestrictedKeywordId = categoryRestrictedKeywordId;
    this.vendorErrorFields = vendorErrorFields;
    this.profileResponse = profileResponse;
    this.categoryResponses = categoryResponses;
  }
}
