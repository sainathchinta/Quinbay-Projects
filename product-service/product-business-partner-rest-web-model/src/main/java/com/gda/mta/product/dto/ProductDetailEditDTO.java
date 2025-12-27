package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.response.InternalProductHistoryEventModel;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Builder
@Data
@ToString
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductDetailEditDTO implements ProductEditContext {
  private String productSku;
  private boolean contentChanged;
  private boolean contentUpdateForUrl;
  private boolean contentUpdateForSpecialAttributes;
  private boolean freeSampleFlagChanged;
  private boolean off2OnChannelFlagChanged;
  private boolean dimensionUpdated;
  private boolean productTypeChanged;
  private boolean preOrderChange;
  private boolean autoCategoryChange;
  private boolean brandChanged;
  private Boolean productEditable;
  private CategoryResponse categoryResponse;
  private ProductDetailResponse productDetailResponse;
  Boolean lateFulfillment;
  String oldCategoryName;
  boolean publishImageQcForContentChange;
  List<RestrictedKeywordsByField> restrictedKeywordsByFieldList = new ArrayList<>();
  RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
    new RestrictedKeywordsByFieldAndActionType(new ArrayList<>());
  private PreOrderRequest preOrderRequest;
  private boolean autoApproved;
  private String contentType;
  private boolean takeDownProduct;
  InternalProductHistoryEventModel internalProductHistoryEventModel;
  ProductHistory productHistory;
  ProductRequest productRequestForPCB;
  ProductCollectionDTO productCollectionDTO;
  ProductBusinessPartner productBusinessPartner;
  ProductDetailPageEditRequest productDetailEditRequestForXProduct;
  List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests = new ArrayList<>();
  AutoApprovalType autoApprovalType;
  List<ProductImageEditRequest> productImageEditRequests = new ArrayList<>();
  DimensionRefreshRequest dimensionRefreshRequest;
  ProductLevel3 productLevel3;
  ProductLevel3DetailResponse productLevel3DetailResponse;
  List<String> reviewTypeList = new ArrayList<>();
  boolean eligibleForAutoReject;
  List<String> allModifiedFields = new ArrayList<>();
  EditChangeType editChangeType;
  boolean eligibleForPCBUpdate;
  List<Map<String, String>> historyForUPC = new ArrayList<>();
  boolean productNameChanged;
  boolean bopisEligible;
  private Boolean videoUpdated;
  private String accessChannel;
}
