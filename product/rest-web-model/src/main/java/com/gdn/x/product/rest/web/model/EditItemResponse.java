package com.gdn.x.product.rest.web.model;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class EditItemResponse extends BaseResponse {
  private static final long serialVersionUID = 4191940372657916747L;
  private ApiErrorCode apiErrorCode;
  private Boolean isArchived;
  private boolean cncActivated;
  private boolean fbbFlagChangedAtL3Level;
  private List<ItemPickupPoint> updatedItemPickupPoints = new ArrayList<ItemPickupPoint>();
  private List<Item> updatedItems = new ArrayList<Item>();
  private Long l3Version;
  private List<ItemPickupPoint> allUpdatedItemPickupPoints = new ArrayList<>();
  private Boolean addOrDeletePPPerformed = false;

  public EditItemResponse(ApiErrorCode apiErrorCode, Boolean isArchived, boolean cncActivated,
      List<ItemPickupPoint> updatedItemPickupPoints, List<Item> updatedItems) {
    this.apiErrorCode = apiErrorCode;
    this.isArchived = isArchived;
    this.cncActivated = cncActivated;
    this.updatedItemPickupPoints = updatedItemPickupPoints;
    this.updatedItems = updatedItems;
  }

  public EditItemResponse(ApiErrorCode apiErrorCode, Boolean isArchived, boolean cncActivated,
      List<ItemPickupPoint> updatedItemPickupPoints, List<Item> updatedItems, Long l3Version) {
    this.apiErrorCode = apiErrorCode;
    this.isArchived = isArchived;
    this.cncActivated = cncActivated;
    this.updatedItemPickupPoints = updatedItemPickupPoints;
    this.updatedItems = updatedItems;
    this.l3Version = l3Version;
  }
}