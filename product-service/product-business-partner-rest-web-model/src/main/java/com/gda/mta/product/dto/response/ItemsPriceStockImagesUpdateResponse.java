package com.gda.mta.product.dto.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.EditedResizeAndImagesUpdateStatusResponse;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.enums.ApiErrorCode;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemsPriceStockImagesUpdateResponse extends BaseResponse {

  private static final long serialVersionUID = 8338182712070124726L;

  private boolean productReview;
  private boolean postLive;
  private ApiErrorCode apiErrorCode;
  private List<VariantsErrorListResponse> variantsErrorList = new ArrayList<>();
  private boolean takeDown;
  private boolean newImagesAdded = false;
  private Long l3Version;
  private String reviewType;
  private EditedResizeAndImagesUpdateStatusResponse editedResizeAndImagesUpdateStatusResponse;
  private Boolean scheduleRemovedForStatusUpdate;

  public ItemsPriceStockImagesUpdateResponse(boolean productReview, boolean postLive, ApiErrorCode apiErrorCode,
      List<VariantsErrorListResponse> variantsErrorList, boolean takeDown) {
    this.productReview = productReview;
    this.postLive = postLive;
    this.apiErrorCode = apiErrorCode;
    this.variantsErrorList = variantsErrorList;
    this.takeDown = takeDown;
  }

  public ItemsPriceStockImagesUpdateResponse(boolean productReview, boolean postLive, ApiErrorCode apiErrorCode,
      List<VariantsErrorListResponse> variantsErrorList, boolean takeDown, boolean newImagesAdded) {
    this.productReview = productReview;
    this.postLive = postLive;
    this.apiErrorCode = apiErrorCode;
    this.variantsErrorList = variantsErrorList;
    this.takeDown = takeDown;
    this.newImagesAdded = newImagesAdded;
  }

  public ItemsPriceStockImagesUpdateResponse(boolean productReview, boolean postLive, ApiErrorCode apiErrorCode,
      List<VariantsErrorListResponse> variantsErrorList, boolean takeDown, boolean newImagesAdded, Long l3Version,
      String reviewType) {
    this.productReview = productReview;
    this.postLive = postLive;
    this.apiErrorCode = apiErrorCode;
    this.variantsErrorList = variantsErrorList;
    this.takeDown = takeDown;
    this.newImagesAdded = newImagesAdded;
    this.l3Version = l3Version;
    this.reviewType = reviewType;
  }
}
